{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.DUAL.Internal
-- Copyright   :  (c) 2011-2015 dual-tree team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module provides access to all of the internals of the
-- DUAL-tree implementation.  Depend on the internals at your own
-- risk!  For a safe public API (and complete documentation), see
-- "Data.Tree.DUAL".
--
-- The main things exported by this module which are not exported from
-- "Data.Tree.DUAL" is one extra type used in the implementation of
-- 'DUALTree', along with functions for manipulating them. A type of
-- trees without up annotations, 'NE', is defined. A 'DUALTree'
-- is a 'NE' with a top-level @u@ annotation along with a
-- possible 'EmptyDUAL'. This method has the advantage that the type
-- system enforces the invariant that there is only one representation
-- for the empty tree. It also allows us to get away with only
-- 'Semigroup' constraints in many places.
--
-----------------------------------------------------------------------------

module Data.Tree.DUAL.Internal
  (
    -- * DUAL-trees
    NE(..), DUALTree(..)

    -- * Constructing DUAL-trees
  , _u, leaf, leafU, down, annot

    -- * Folding DUAL-trees
  , foldDUAL
  , foldDUAL'
  , flatten

    -- * Up annotations
  -- , _u
  , getU
  , mapU
  , preapplyU
  , postapplyU

  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.DeepSeq
import           Data.Foldable             as F (foldMap)
import           Data.Monoid.Action
import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Data.Sequence
import           Data.Typeable

------------------------------------------------------------------------
-- Non-Empty tree
------------------------------------------------------------------------

-- | Non-Empty DUAL-tree. u annotations are included so parts of the
--   tree can be edited and the u annotations can be rebuilt properly.
data NE d u a l
  = Leaf   u !l                  -- ^ @l@eaf
  | Down   u !d !(NE d u a l)    -- ^ @d@own-annotation
  | Annot  u !a !(NE d u a l)    -- ^ @a@nnotation
  | Concat u !(Seq (NE d u a l)) -- ^ n-way branch
  deriving (Functor, Typeable, Show, Eq)

gu :: NE d u a l -> u
gu = \case
  Leaf   u _   -> u
  Down   u _ _ -> u
  Annot  u _ _ -> u
  Concat u _   -> u

fu :: (u -> u') -> NE d u a l -> NE d u' a l
fu f = go where
  go = \case
    Leaf u l    -> Leaf (f u) l
    Down u d t  -> Down (f u) d (go t)
    Annot u a t -> Annot (f u) a (go t)
    Concat u ts -> Concat (f u) (fmap go ts)

instance (Semigroup d, Semigroup u) => Semigroup (NE d u a l) where
  Concat u1 t1 <> Concat u2 t2  = Concat (u1 <> u2)    (t1 <> t2)
  Concat u1 t1 <> t2            = Concat (u1 <> gu t2) (t1 |> t2)
  t1           <> Concat u2 t2  = Concat (gu t1 <> u2) (t1 <| t2)
  t1           <> t2            = Concat (gu t1 <> gu t2) (fromList [t1,t2])

instance (NFData d, NFData u, NFData a, NFData l) => NFData (NE d u a l) where
  rnf (Leaf u l)    = rnf u `seq` rnf l
  rnf (Down u d t)  = rnf u `seq` rnf d `seq` rnf t
  rnf (Annot u a t) = rnf u `seq` rnf a `seq` rnf t
  rnf (Concat u s)  = rnf u `seq` rnf s

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
  = DUALTree !(NE d u a l)
  | EmptyDUAL
  deriving (Functor, Typeable, Show, Eq)

instance (Semigroup u, Semigroup d) => Semigroup (DUALTree d u a l) where
  DUALTree  t1 <> DUALTree t2 = DUALTree (t1 <> t2)
  EmptyDUAL    <> a           = a
  a            <> EmptyDUAL   = a

instance (Semigroup u, Semigroup d) => Monoid (DUALTree d u a l) where
  mappend = (<>)
  mempty  = EmptyDUAL

instance (NFData d, NFData u, NFData a, NFData l) => NFData (DUALTree d u a l) where
  rnf (DUALTree u t) = rnf u `seq` rnf t
  rnf _              = ()

------------------------------------------------------------
-- Convenience methods etc.
------------------------------------------------------------

-- | Traversal over all up annotations.
_u :: Applicative f => (u -> f u') -> DUALTree d u a l -> f (DUALTree d u' a l)
_u f (DUALTree t0) = DUALTree <$> go t0 where
  go = \case
    Leaf u l    -> Leaf <$> f u <*> pure l
    Down u d t  -> Down <$> f u <*> pure d <*> go t
    Annot u a t -> Annot <$> f u <*> pure a <*> go t
    Concat u ts -> Concat <$> f u <*> traverse go ts
_u _ _            = pure EmptyDUAL

-- | Construct a leaf node from a @u@ annotation along with a leaf.
leaf :: u -> l -> DUALTree d u a l
leaf u l = DUALTree (Leaf u l)

-- | Construct an DUALTree that only contains a @u@ annotation.
leafU :: u -> DUALTree d u a l
leafU u = DUALTree (Concat u mempty)

-- | Add an internal data value at the root of a tree.  Note that this
--   only works on /non-empty/ trees; on empty trees this function is
--   the identity. O(1)
annot :: a -> DUALTree d u a l -> DUALTree d u a l
annot _ EmptyDUAL    = EmptyDUAL
annot a (DUALTree t) = DUALTree (Annot (gu t) a t)

-- | Apply a @d@ annotation at the root of a tree, transforming all
--   @u@ annotations by the action of @d@.
down :: (Semigroup d, Semigroup u, Action d u) => d -> DUALTree d u a l -> DUALTree d u a l
down _ EmptyDUAL      = EmptyDUAL
down d (DUALTree t) = DUALTree $ case t of
  Down _ d' t' -> Down u (d <> d') t'
  _            -> Down u d t
  where u = act d (gu t)

-- | Get the up annotation of a non-empty DUALTree.
getU :: DUALTree d u a l -> Maybe u
getU (DUALTree t) = Just (gu t)
getU _            = Nothing

-- | Map over the @u@ annotation of a DUALTree.
--
--   If you want 'mapU' to commute with monoid composition, that is,
--   @mapU f (d1 \<\> d2) === mapU f d1 \<\> mapU f d2@, it suffices
--   to ensure that @f@ is a monoid homomorphism, that is, @f mempty =
--   mempty@ and @f (u1 \<\> u2) = f u1 \<\> f u2@.  Additionally,
--   @mapU f@ will commute with @act d@ if @f@ does.
mapU :: (u -> u') -> DUALTree d u a l -> DUALTree d u' a l
mapU f (DUALTree t) = DUALTree (fu f t)
mapU _ _            = EmptyDUAL

-- | Apply a @u@ annotation of a DUALTree on the left. Makes a 'leafU'
--   for an empty tree.
preapplyU :: Semigroup u => u -> DUALTree d u a l -> DUALTree d u a l
preapplyU u' (DUALTree t0) = DUALTree $
  case t0 of
    Leaf u l    -> Leaf (u' <> u) l
    Down u d t  -> Down (u' <> u) d t
    Annot u a t -> Annot (u' <> u) a t
    Concat u ts -> Concat (u' <> u) ts
preapplyU u' _            = leafU u'

-- | Apply an @u@ annotation of a DUALTree on the right. Makes a 'leafU'
--   for an empty tree.
postapplyU :: Semigroup u => u -> DUALTree d u a l -> DUALTree d u a l
postapplyU u' (DUALTree t0) = DUALTree $
  case t0 of
    Leaf u l    -> Leaf (u <> u') l
    Down u d t  -> Down (u <> u') d t
    Annot u a t -> Annot (u <> u') a t
    Concat u ts -> Concat (u <> u') ts
postapplyU u' _              = leafU u'

------------------------------------------------------------
-- Folds
------------------------------------------------------------

-- | Fold a dual tree for a monoidal result @r@. The @d@ annotations are
--   accumulated from the top of the tree. Static @a@ annotations are
--   acted on by the @d@ annotation accumulated up to that point.
foldDUAL :: (Action d a, Monoid' d, Monoid r)
         => (d -> l -> r) -- ^ Process a leaf
         -> (a -> r -> r) -- ^ Process an annotation
         -> DUALTree d u a l
         -> r
foldDUAL _  _  EmptyDUAL       = mempty
foldDUAL lF aF (DUALTree t0) = go mempty t0
  where
    go !d = \case
      Down _ d' t -> go (d <> d') t
      Leaf _ l    -> lF d l
      Annot _ a t -> aF (act d a) (go d t)
      Concat _ ts -> F.foldMap (go d) ts
{-# INLINE foldDUAL #-}

-- | Similar to 'foldDUAL', but with access to /partial/ down
--   annotations at @Concat@ nodes and @Leaf@ nodes, as well as
--   complete accumulated down annotations at leaves, as with
--   'foldDUAL'.  At each @Concat@ node and each @Leaf@, the @(d -> d
--   -> p)@ function is given access to the total accumulated down
--   annotation from the root, as well as the partially accumulated
--   value since the nearest parent @Concat@ node.  The resulting @p@
--   value will be processed by the @(p -> r -> r)@ function.
foldDUAL'
  :: (Action d a, Monoid' d, Monoid r)
  => (d -> l -> r) -- ^ Process a leaf with total and local accumulation of down
  -> (a -> r -> r) -- ^ Process an annotation
  -> (d -> d -> p) -- ^ Given fully accumulated and partially
                   --   accumulated down annotation, produce a partial
                   --   down annotation @p@
  -> (p -> r -> r) -- ^ Process a partial down annotation
  -> DUALTree d u a l
  -> r
foldDUAL' _  _  _   _  EmptyDUAL     = mempty
foldDUAL' lF aF mkP pF (DUALTree t0) = go mempty mempty t0
  where
    -- d is the total accumulated down annotations before the last Concat
    -- w is the down annotations since the last Concat
    -- dw is the total accumulated down annotations
    -- p is the partial annotation since the last Concat
    -- at every Concat, the partial annotation is applied and w is reset
    go !d w = \case
      Down _ d' t -> go d (w <> d') t
      Leaf _ l    -> pF p $ lF dw l
      Annot _ a t -> aF (act dw a) (go d w t)
      Concat _ ts -> pF p $ F.foldMap (go dw mempty) ts
      where p  = mkP d w
            dw = d <> w
{-# INLINE foldDUAL' #-}

-- | A specialized fold provided for convenience: flatten a tree into
--   a list of leaves along with their @d@ annotations, ignoring
--   internal data values.
flatten :: (Action d a, Monoid' d) => DUALTree d u a l -> [(l, d)]
flatten = foldDUAL (\d l -> [(l, d)]) (flip const)

