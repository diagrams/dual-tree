{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Data.Functor
import           Data.Maybe
import           Data.Typeable

import           Test.QuickCheck.All (quickCheckAll)
import           Test.QuickCheck hiding ((===))
import           Test.Feat

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import           Data.Monoid.Action
import           Data.Semigroup
import           Data.Tree.DUAL

data DUALTreeExpr d u a l =
    EEmpty
  | ELeaf u l
  | ELeafU u
  | EConcat (NonEmpty (DUALTreeExpr d u a l))
  | EAct d (DUALTreeExpr d u a l)
  | EAnnot a (DUALTreeExpr d u a l)
  deriving (Show, Typeable)

deriveEnumerable ''NonEmpty
deriveEnumerable ''DUALTreeExpr

buildTree :: (Semigroup u, Semigroup d, Action d u)
          => DUALTreeExpr d u a l -> DUALTree d u a l
buildTree EEmpty       = empty
buildTree (ELeaf u l)  = leaf u l
buildTree (ELeafU u)   = leafU u
buildTree (EConcat ts) = sconcat (NEL.map buildTree ts)
buildTree (EAct d t)   = applyD d (buildTree t)
buildTree (EAnnot a t) = annot a (buildTree t)

-- buildTree' :: DUALTreeExpr D U () Bool -> DUALTree D U () Bool
-- buildTree' = buildTree

instance Num a => Action (Product a) (Sum a) where
  act (Product p) (Sum s) = Sum (p * s)

type U = Sum Int
type D = Product Int

deriving instance Typeable1 Sum
deriving instance Typeable1 Product

deriveEnumerable ''Sum
deriveEnumerable ''Product

type T = DUALTree D U Bool Bool

instance Arbitrary T where
  arbitrary = buildTree <$> sized uniform

prop_leaf_u :: U -> Bool
prop_leaf_u u = getU (leaf u ()) == Just u

prop_leafU_u :: U -> Bool
prop_leafU_u u = getU (leafU u) == Just u

prop_applyUpre :: U -> T -> Bool
prop_applyUpre u t = getU (applyUpre u t) == Just (u <> fromMaybe mempty (getU t))

prop_applyUpost :: U -> T -> Bool
prop_applyUpost u t = getU (applyUpost u t) == Just (fromMaybe mempty (getU t) <> u)

--------------------------------------------------
-- Monoid laws
--------------------------------------------------

prop_mempty_idL :: T -> Bool
prop_mempty_idL t = mempty <> t == t

prop_mempty_idR :: T -> Bool
prop_mempty_idR t = t <> mempty == t

infix 4 ===
t1 === t2 = flatten t1 == flatten t2

-- mappend is associative up to flattening.
prop_mappend_assoc :: T -> T -> T -> Bool
prop_mappend_assoc t1 t2 t3 = (t1 <> t2) <> t3 === t1 <> (t2 <> t3)

--------------------------------------------------
-- Action laws
--------------------------------------------------

prop_mempty_act :: T -> Bool
prop_mempty_act t = applyD mempty t === t

prop_mappend_act :: D -> D -> T -> Bool
prop_mappend_act d1 d2 t = applyD d1 (applyD d2 t) == applyD (d1 <> d2) t

prop_act_mempty :: D -> Bool
prop_act_mempty d = applyD d (mempty :: T) == mempty

prop_act_mappend :: D -> T -> T -> Bool
prop_act_mappend d t1 t2 = applyD d (t1 <> t2) === applyD d t1 <> applyD d t2

return []
main = $quickCheckAll
