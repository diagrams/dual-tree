{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}

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
    DALTree(..), DUALTree(..)

    -- * Constructing DUAL-trees
  , leaf, leafU, down, annot

    -- * Folding DUAL-trees
  , foldDUAL
  , flatten

    -- * Up annotations
  , _u
  , getU
  , mapU
  , preapplyU
  , postapplyU

  ) where

import Control.Applicative
import Data.Monoid.Action
import Data.Semigroup
import Data.Typeable
import Data.Monoid.WithSemigroup
import Control.DeepSeq
import Data.Sequence
import Data.Foldable as F (foldMap)

------------------------------------------------------------------------
-- DALTree
------------------------------------------------------------------------

-- | /Non-empty/ DUAL-tree without the u.
data DALTree d a l
  = Leaf   !l                  -- ^ @l@eaf
  | Down   !d !(DALTree d a l) -- ^ @d@own-annotation
  | Annot  !a !(DALTree d a l) -- ^ @a@nnotation
  | Concat !(Seq (DALTree d a l)) -- ^ n-way branch
  deriving (Functor, Typeable, Show, Eq)

instance Semigroup d => Semigroup (DALTree d a l) where
  Concat t1  <> Concat t2  = Concat (t1 <> t2)
  Concat t1  <> t2         = Concat (t1 |> t2)
  t1         <> Concat t2  = Concat (t1 <| t2)
  t1         <> t2         = Concat (fromList [t1,t2])

instance (NFData d, NFData a, NFData l) => NFData (DALTree d a l) where
  rnf (Leaf l)    = rnf l
  rnf (Down d t)  = rnf d `seq` rnf t
  rnf (Annot a t) = rnf a `seq` rnf t
  rnf (Concat s)  = rnf s

------------------------------------------------------------------------
-- DUALTree
------------------------------------------------------------------------

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

-- | A non-empty DUAL-tree paired with a cached @u@ value.  These
--   should never be constructed directly; instead, use 'pullU'.
data DUALTree d u a l
  = DUALTree !u !(DALTree d a l)
  | EmptyDUAL
  deriving (Functor, Typeable, Show, Eq)

instance (Semigroup u, Semigroup d) => Semigroup (DUALTree d u a l) where
  DUALTree u1 t1 <> DUALTree u2 t2 = DUALTree (u1 <> u2) (t1 <> t2)
  EmptyDUAL <> a = a
  a <> EmptyDUAL = a

instance (Semigroup u, Semigroup d) => Monoid (DUALTree d u a l) where
  mappend = (<>)
  mempty  = EmptyDUAL

instance (NFData d, NFData u, NFData a, NFData l) => NFData (DUALTree d u a l) where
  rnf (DUALTree u t) = rnf u `seq` rnf t
  rnf _              = ()

------------------------------------------------------------
-- Convenience methods etc.
------------------------------------------------------------

-- | Traversal over the up annotation.
_u :: Applicative f => (u -> f u') -> DUALTree d u a l -> f (DUALTree d u' a l)
_u f (DUALTree u t) = fmap (\u' -> DUALTree u' t) (f u)
_u _ _              = pure EmptyDUAL

-- | Construct a leaf node from a @u@ annotation along with a leaf.
leaf :: u -> l -> DUALTree d u a l
leaf u l = DUALTree u (Leaf l)

-- | Construct a leaf node from a @u@ annotation along with a leaf.
leafU :: u -> DUALTree d u a l
leafU u = DUALTree u (Concat mempty)

-- | Add an internal data value at the root of a tree.  Note that this
--   only works on /non-empty/ trees; on empty trees this function is
--   the identity.
annot :: (Semigroup u, Action d u) => a -> DUALTree d u a l -> DUALTree d u a l
annot _ EmptyDUAL      = EmptyDUAL
annot a (DUALTree u t) = DUALTree u (Annot a t)

-- | Apply a @d@ annotation at the root of a tree, transforming all
--   @u@ annotations by the action of @d@.
down :: (Semigroup d, Semigroup u, Action d u) => d -> DUALTree d u a l -> DUALTree d u a l
down _ EmptyDUAL      = EmptyDUAL
down d (DUALTree u t) = DUALTree (act d u) $ case t of
  Down d' t' -> Down (d <> d') t'
  _          -> Down d t

-- | Get the up annotation of a non-empty DUALTree.
getU :: DUALTree d u a l -> Maybe u
getU (DUALTree u _) = Just u
getU _              = Nothing

-- | Get the up annotation of a non-empty DUALTree.
mapU :: (u -> u') -> DUALTree d u a l -> DUALTree d u' a l
mapU f (DUALTree u t) = DUALTree (f u) t
mapU _ _              = EmptyDUAL

-- | Get the up annotation of a non-empty DUALTree.
preapplyU :: Semigroup u => u -> DUALTree d u a l -> DUALTree d u a l
preapplyU u (DUALTree u' t) = DUALTree (u' <> u) t
preapplyU u _               = leafU u

-- | Get the up annotation of a non-empty DUALTree.
postapplyU :: Semigroup u => u -> DUALTree d u a l -> DUALTree d u a l
postapplyU u (DUALTree u' t) = DUALTree (u <> u') t
postapplyU u _               = leafU u

------------------------------------------------------------
-- Folds
------------------------------------------------------------

-- | Fold a dual tree using the
foldDUAL :: (Monoid' d, Monoid r)
         => (d -> l -> r)      -- ^ Process a leaf
         -> (d -> a -> r -> r) -- ^ Process an anotation
         -> DUALTree d u a l
         -> r
foldDUAL _  _  EmptyDUAL       = mempty
foldDUAL lF aF (DUALTree _ t0) = go mempty t0
  where
    go d = \case
      Down d' t  -> go (d <> d') t
      Leaf l     -> lF d l
      Annot a t  -> aF d a (go d t)
      Concat ts  -> F.foldMap (go d) ts
{-# INLINE foldDUAL #-}

-- | A specialized fold provided for convenience: flatten a tree into
--   a list of leaves along with their @d@ annotations, ignoring
--   internal data values.
flatten :: Monoid' d => DUALTree d u a l -> [(l, d)]
flatten = foldDUAL (\d l -> [(l, d)]) (\_ _ r -> r)

