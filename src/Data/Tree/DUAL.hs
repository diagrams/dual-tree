
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.DUAL
-- Copyright   :  (c) 2011-2012 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Rose (n-ary) trees with both upwards- (/i.e./ cached) and
-- downwards-traveling (/i.e./ accumulating) monoidal annotations.
-- This is used as the core data structure underlying the @diagrams@
-- framework (<http://projects.haskell.org/diagrams>), but potentially
-- has other applications as well.
--
-- Abstractly, a DUALTree is a rose (n-ary) tree with data (of type
-- @l@) at leaves, data (of type @a@) at internal nodes, and two types
-- of monoidal annotations, one (of type @u@) travelling \"up\" the
-- tree and one (of type @d@) traveling \"down\".
--
-- Specifically, there are five types of nodes:
--
--   * Leaf nodes which contain a data value of type @l@ and an
--     annotation of type @u@.  The annotation represents information
--     about a tree that should be accumulated (/e.g./ number of
--     leaves, some sort of \"weight\", /etc./).  If you are familiar
--     with finger trees
--     (<http://www.soi.city.ac.uk/~ross/papers/FingerTree.html>,
--     <http://hackage.haskell.org/package/fingertree>), it is the
--     same idea.
--
--   * There is also a special type of leaf node which contains only a
--     @u@ value, and no data. This allows cached @u@ values to be
--     \"modified\" by inserting extra annotations.
--
--   * Branch nodes, containing a list of subtrees.
--
--   * Internal nodes with a value of type @d@.  @d@ may have an
--     /action/ on @u@ (see the 'Action' type class, defined in
--     "Data.Monoid.Action" from the @monoid-extras@ package).
--     Semantically speaking, applying a @d@ annotation to a tree
--     transforms all the @u@ annotations below it by acting on them.
--     Operationally, however, since the action must be a monoid
--     homomorphism, applying a @d@ annotation can actually be done in
--     constant time.
--
--   * Internal nodes with data values of type @a@, possibly of a
--     different type than those in the leaves.  These are just \"along
--     for the ride\" and are unaffected by @u@ and @d@ annotations.
--
-- There are two critical points to note about @u@ and @d@ annotations:
--
--   * The combined @u@ annotation for an entire tree is always cached
--     at the root and available in constant (amortized) time.
--
--   * The 'mconcat' of all the @d@ annotations along the path from
--     the root to each leaf is available along with the leaf during a
--     fold operation.
--
-- A fold over a @DUALTree@ is given access to the internal and leaf
-- data, and the accumulated @d@ values at each leaf.  It is also
-- allowed to replace \"@u@-only\" leaves with a constant value.  In
-- particular, however, it is /not/ given access to any of the @u@
-- annotations, the idea being that those are used only for
-- /constructing/ trees.  It is also not given access to @d@ values as
-- they occur in the tree, only as they accumulate at leaves.  If you
-- do need access to @u@ or @d@ values, you can duplicate the values
-- you need in the internal data nodes.
--
-----------------------------------------------------------------------------

module Data.Tree.DUAL
  (
    -- * DUAL-trees
    DUALTree

    -- * Constructing DUAL-trees
  , leaf, annot, down

    -- * Modifying DUAL-trees
  , _u, mapU

    -- * Accessors and eliminators
  , getU, foldDUAL, flatten

  ) where

import Data.Tree.DUAL.Internal

