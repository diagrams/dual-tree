{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveDataTypeable         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.DUAL.Internal
-- Copyright   :  (c) 2011-2012 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module provides access to all of the internals of the
-- DUAL-tree implementation.  Depend on the internals at your own
-- risk!  For a safe public API (and complete documentation), see
-- "Data.Tree.DUAL".
--
-- The main things exported by this module which are not exported from
-- "Data.Tree.DUAL" are two extra types used in the implementation of
-- 'DUALTree', along with functions for manipulating them.  A type of
-- /non-empty/ trees, 'DUALTreeNE', is defined, as well as the type
-- 'DUALTreeU' which represents a non-empty tree paired with a cached
-- @u@ annotation.  'DUALTreeNE' and 'DUALTreeU' are mutually
-- recursive, so that recursive tree nodes are interleaved with cached
-- @u@ annotations.  'DUALTree' is defined by just wrapping
-- 'DUALTreeU' in 'Option'.  This method has the advantage that the
-- type system enforces the invariant that there is only one
-- representation for the empty tree.  It also allows us to get away
-- with only 'Semigroup' constraints in many places.
--
-----------------------------------------------------------------------------

module Data.Tree.DUAL.Internal
       (
         -- * DUAL-trees
         DUALTreeNE(..), DUALTreeU(..), DUALTree(..)

         -- * Constructing DUAL-trees
       , empty, leaf, leafU, annot, applyD

         -- * Modifying DUAL-trees
       , applyUpre, applyUpost
       , mapUNE, mapUU, mapU

         -- * Accessors and eliminators
       , nonEmpty, getU, foldDUALNE, foldDUAL, flatten

       ) where

import           Control.Arrow ((***))
import           Data.Functor ((<$>))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import           Data.Monoid.Action
import           Data.Semigroup
import           Data.Tuple (swap)
import           Data.Typeable

import           Control.Newtype

------------------------------------------------------------
-- DUALTreeNE
------------------------------------------------------------

-- | /Non-empty/ DUAL-trees.
data DUALTreeNE d u a l
  = Leaf   u l        -- ^ Leaf with data value and @u@ annotation
  | LeafU  u          -- ^ Leaf with only @u@ annotation
  | Concat (NonEmpty (DUALTreeU d u a l))
                      -- ^ n-way branch, containing a /non-empty/ list
                      --   of subtrees.
  | Act    d (DUALTreeU d u a l)
                      -- ^ @d@ annotation
  | Annot  a (DUALTreeU d u a l)
                      -- ^ Internal data value
  deriving (Functor, Typeable, Show, Eq)

instance (Action d u, Semigroup u) => Semigroup (DUALTreeNE d u a l) where
  t1 <> t2   = sconcat (NEL.fromList [t1,t2])
  sconcat    = Concat . NEL.map pullU

newtype DAct d = DAct { unDAct :: d }

instance Newtype (DAct d) d where
  pack   = DAct
  unpack = unDAct

instance (Semigroup d, Semigroup u, Action d u)
    => Action (DAct d) (DUALTreeNE d u a l) where
  act (DAct d) (Act d' t) = Act (d <> d') t
  act (DAct d) t          = Act d (pullU t)

------------------------------------------------------------
-- DUALTreeU
------------------------------------------------------------

-- | A non-empty DUAL-tree paired with a cached @u@ value.  These
--   should never be constructed directly; instead, use 'pullU'.
newtype DUALTreeU d u a l = DUALTreeU { unDUALTreeU :: (u, DUALTreeNE d u a l) }
  deriving (Functor, Semigroup, Typeable, Show, Eq)

instance Newtype (DUALTreeU d u a l) (u, DUALTreeNE d u a l) where
  pack   = DUALTreeU
  unpack = unDUALTreeU

instance (Semigroup d, Semigroup u, Action d u)
    => Action (DAct d) (DUALTreeU d u a l) where
  act d = over DUALTreeU (act (unDAct d) *** act d)

-- | \"Pull\" the root @u@ annotation out into a tuple.
pullU :: (Semigroup u, Action d u) => DUALTreeNE d u a l -> DUALTreeU d u a l
pullU t@(Leaf u _)                   = pack (u, t)
pullU t@(LeafU u)                    = pack (u, t)
pullU t@(Concat ts)                  = pack (sconcat . NEL.map (fst . unpack) $ ts, t)
pullU t@(Act d (DUALTreeU (u,_)))    = pack (act d u, t)
pullU t@(Annot _ (DUALTreeU (u, _))) = pack (u, t)

------------------------------------------------------------
-- DUALTree
------------------------------------------------------------

-- | Rose (n-ary) trees with both upwards- (/i.e./ cached) and
--   downwards-traveling (/i.e./ accumulating) monoidal annotations.
--   Abstractly, a DUALTree is a rose (n-ary) tree with data (of type
--   @l@) at leaves, data (of type @a@) at internal nodes, and two
--   types of monoidal annotations, one (of type @u@) travelling
--   \"up\" the tree and one (of type @d@) traveling \"down\".  See
--   the documentation at the top of this file for full details.
--
--   @DUALTree@ comes with some instances:
--
--   * 'Functor', for modifying leaf data.  Note that 'fmap' of course
--     cannot alter any @u@ annotations.
--
--   * 'Semigroup'. @DUALTreeNE@s form a semigroup where @(\<\>)@
--     corresponds to adjoining two trees under a common parent root,
--     with @sconcat@ specialized to put all the trees under a single
--     parent.  Note that this does not satisfy associativity up to
--     structural equality, but only up to observational equivalence
--     under 'flatten'.  Technically using 'foldDUAL' directly enables
--     one to observe the difference, but it is understood that
--     'foldDUAL' should be used only in ways such that reassociation
--     of subtrees \"does not matter\".
--
--   * 'Monoid'. The identity is the empty tree.

newtype DUALTree d u a l = DUALTree { unDUALTree :: Option (DUALTreeU d u a l) }
  deriving ( Functor, Semigroup, Typeable, Show, Eq )

instance Newtype (DUALTree d u a l) (Option (DUALTreeU d u a l)) where
  pack   = DUALTree
  unpack = unDUALTree

instance (Semigroup u, Action d u) => Monoid (DUALTree d u a l) where
  mempty  = DUALTree mempty
  mappend = (<>)
  mconcat []     = mempty
  mconcat (x:xs) = sconcat (x :| xs)

-- | Apply a @d@ annotation at the root of a tree.  Semantically, all
--   @u@ annotations are transformed by the action of @d@, although
--   operationally @act@ incurs only a constant amount of work.
instance (Semigroup d, Semigroup u, Action d u)
    => Action (DAct d) (DUALTree d u a l) where
  act = over DUALTree . fmap . act

------------------------------------------------------------
-- Convenience methods etc.
------------------------------------------------------------

-- | The empty DUAL-tree.  This is a synonym for 'mempty', but with a
--   more general type.
empty :: DUALTree d u a l
empty = DUALTree (Option Nothing)

-- | Construct a leaf node from a @u@ annotation along with a leaf
--   datum.
leaf :: u -> l -> DUALTree d u a l
leaf u l = DUALTree (Option (Just (DUALTreeU (u, Leaf u l))))

-- | Construct a leaf node from a @u@ annotation.
leafU :: u -> DUALTree d u a l
leafU u = DUALTree (Option (Just (DUALTreeU (u, LeafU u))))

-- | Add a @u@ annotation to the root, combining it (on the left) with
--   the existing cached @u@ annotation.  This function is provided
--   just for convenience; @applyUpre u t = 'leafU' u \<\> t@.
applyUpre :: (Semigroup u, Action d u) => u -> DUALTree d u a l -> DUALTree d u a l
applyUpre u t = leafU u <> t

-- | Add a @u@ annotation to the root, combining it (on the right) with
--   the existing cached @u@ annotation.  This function is provided
--   just for convenience; @applyUpost u t = t \<\> 'leafU' u@.
applyUpost :: (Semigroup u, Action d u) => u -> DUALTree d u a l -> DUALTree d u a l
applyUpost u t = t <> leafU u

-- | Add an internal data value at the root of a tree.  Note that this
--   only works on /non-empty/ trees; on empty trees this function is
--   the identity.
annot :: (Semigroup u, Action d u) => a -> DUALTree d u a l -> DUALTree d u a l
annot a = (over DUALTree . fmap) (pullU . Annot a)

-- | Apply a @d@ annotation at the root of a tree, transforming all
--   @u@ annotations by the action of @d@.
applyD :: (Semigroup d, Semigroup u, Action d u)
       => d -> DUALTree d u a l -> DUALTree d u a l
applyD = act . DAct

-- | Decompose a DUAL-tree into either @Nothing@ (if empty) or a
--   top-level cached @u@ annotation paired with a non-empty
--   DUAL-tree.
nonEmpty :: DUALTree d u a l -> Maybe (u, DUALTreeNE d u a l)
nonEmpty = fmap unpack . getOption . unpack

-- | Get the @u@ annotation at the root, or @Nothing@ if the tree is
--   empty.
getU :: DUALTree d u a l -> Maybe u
getU = fmap fst . nonEmpty

------------------------------------------------------------
-- Maps
------------------------------------------------------------

-- XXX todo: try adding Map as a constructor, so we can delay the
-- mapping until the end too?

-- | Map a function (which must be a monoid homomorphism, and commute
--   with the action of @d@) over all the @u@ annotations in a non-empty
--   DUAL-tree.
mapUNE :: (u -> u') -> DUALTreeNE d u a l -> DUALTreeNE d u' a l
mapUNE f (Leaf u l)  = Leaf (f u) l
mapUNE f (LeafU u)   = LeafU (f u)
mapUNE f (Concat ts) = Concat ((NEL.map . mapUU) f ts)
mapUNE f (Act d t)   = Act d (mapUU f t)
mapUNE f (Annot a t) = Annot a (mapUU f t)

-- | Map a function (which must be a monoid homomorphism, and commute
--   with the action of @d@) over all the @u@ annotations in a
--   non-empty DUAL-tree paired with its cached @u@ value.
mapUU :: (u -> u') -> DUALTreeU d u a l -> DUALTreeU d u' a l
mapUU f = over DUALTreeU (f *** mapUNE f)

-- | Map a function over all the @u@ annotations in a DUAL-tree.  The
--   function must be a monoid homomorphism, and must commute with the
--   action of @d@ on @u@.  That is, to use @mapU f@ safely it must be
--   the case that
--
--     * @f mempty == mempty@
--
--     * @f (u1 \<\> u2) == f u1 \<\> f u2@
--
--     * @f (act d u) == act d (f u)@
--
mapU :: (u -> u') -> DUALTree d u a l -> DUALTree d u' a l
mapU = over DUALTree . fmap . mapUU

------------------------------------------------------------
-- Folds
------------------------------------------------------------

-- | Fold for non-empty DUAL-trees.
foldDUALNE :: (Semigroup d, Monoid d)
           => (d -> l -> r) -- ^ Process a leaf datum along with the
                            --   accumulation of @d@ values along the
                            --   path from the root
           -> r             -- ^ Replace @LeafU@ nodes
           -> (NonEmpty r -> r)  -- ^ Combine results at a branch node
           -> (a -> r -> r)      -- ^ Process an internal datum
           -> DUALTreeNE d u a l -> r
foldDUALNE  = foldDUALNE' (Option Nothing)
  where
    foldDUALNE' dacc lf _   _   _   (Leaf _ l)  = lf (option mempty id dacc) l
    foldDUALNE' _    _  lfU _   _   (LeafU _)   = lfU
    foldDUALNE' dacc lf lfU con ann (Concat ts)
      = con (NEL.map (foldDUALNE' dacc lf lfU con ann . snd . unpack) ts)
    foldDUALNE' dacc lf lfU con ann (Act d t)
      = foldDUALNE' (dacc <> (Option (Just d))) lf lfU con ann . snd . unpack $ t
    foldDUALNE' dacc lf lfU con ann (Annot a t)
      = ann a (foldDUALNE' dacc lf lfU con ann . snd . unpack $ t)

-- | Fold for DUAL-trees. It is given access to the internal and leaf
--   data, and the accumulated @d@ values at each leaf.  It is also
--   allowed to replace \"@u@-only\" leaves with a constant value.  In
--   particular, however, it is /not/ given access to any of the @u@
--   annotations, the idea being that those are used only for
--   /constructing/ trees.  It is also not given access to @d@ values
--   as they occur in the tree, only as they accumulate at leaves.  If
--   you do need access to @u@ or @d@ values, you can duplicate the
--   values you need in the internal data nodes.
--
--   The result is @Nothing@ if and only if the tree is empty.
foldDUAL :: (Semigroup d, Monoid d)
         => (d -> l -> r)          -- ^ Process a leaf datum along with the
                                   --   accumulation of @d@ values along the
                                   --   path from the root
         -> r                      -- ^ Replace @u@-only nodes
         -> (NonEmpty r -> r)      -- ^ Combine results at a branch node
         -> (a -> r -> r)          -- ^ Process an internal datum
         -> DUALTree d u a l -> Maybe r
foldDUAL _ _ _ _ (DUALTree (Option Nothing))
  = Nothing
foldDUAL l u c a (DUALTree (Option (Just (DUALTreeU (_, t)))))
  = Just $ foldDUALNE l u c a t

-- | A specialized fold provided for convenience: flatten a tree into
--   a list of leaves along with their @d@ annotations, ignoring
--   internal data values.
flatten :: (Semigroup d, Monoid d) => DUALTree d u a l -> [(l, d)]
flatten = fromMaybe []
        . foldDUAL
            (\d l -> [(l, d)])
            []
            (concat . NEL.toList)
            (const id)
