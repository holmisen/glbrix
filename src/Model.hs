{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Primitive
import Types

import Data.Foldable (traverse_)
import Data.Semigroup
import Lens.Micro
import Lens.Micro.Extras (view)

--------------------------------------------------------------------------------

newtype Rotation = Rotation Int deriving (Eq, Num, Ord, Show)

noRotation :: Rotation
noRotation = 0

--------------------------------------------------------------------------------

data Placement = Placement { position :: !P3, rotation :: !Rotation }
  deriving (Eq, Show)

lposition :: Lens' Placement P3
lposition = lens position (\p newPosition -> p { position = newPosition })

lrotation :: Lens' Placement Rotation
lrotation = lens rotation (\p newRotation -> p { rotation = newRotation })

--------------------------------------------------------------------------------

data Placed a = Placed !Placement !Color !a
  deriving Show

lplacement :: Lens' (Placed a) Placement
lplacement = lens (\(Placed p _ _) -> p) (\(Placed _ c a) p -> Placed p c a)

lcolor :: Lens' (Placed a) Color
lcolor = lens (\(Placed _ c _) -> c) (\(Placed p _ a) c -> Placed p c a)

lplacedValue :: Lens' (Placed a) a
lplacedValue = lens (\(Placed _ _ a) -> a) (\(Placed p c _) a -> Placed p c a)

rotatePlaced :: Rotation -> P3 -> Placed a -> Placed a
rotatePlaced r@(Rotation n) refPoint p =
   p & lplacement.lposition %~ rotateAroundPoint n refPoint
     & lplacement.lrotation %~ (+r)

--------------------------------------------------------------------------------

data Tree a = Part a
            | Group [Tree a]
  deriving (Foldable, Functor, Show, Traversable)

groupTrees :: [Tree a] -> Tree a
groupTrees = Group

ungroupTree :: Tree a -> [Tree a]
ungroupTree (Group ns) = ns
ungroupTree n          = [n]

flattenPartTree :: Tree a -> [a]
flattenPartTree (Part a)   = [a]
flattenPartTree (Group ts) = concatMap flattenPartTree ts

--------------------------------------------------------------------------------

type PlacedPart = Tree (Placed Prim)

translatePart :: V3 Int -> PlacedPart -> PlacedPart
translatePart v = traversed.lplacement.lposition %~ translate v

-- | The position of the first part in the tree.
partPosition :: PlacedPart -> P3
partPosition (Part a)   = a ^. lplacement.lposition
partPosition (Group ps) = partPosition (head ps)

setPartColor :: Color -> PlacedPart -> PlacedPart
setPartColor c = traversed.lcolor .~ c

groupParts :: [PlacedPart] -> PlacedPart
groupParts = groupTrees

ungroupPart :: PlacedPart -> [PlacedPart]
ungroupPart = ungroupTree

-- | Get the smallest z position of the part
getPartMinZ :: PlacedPart -> Min Int
getPartMinZ = view (traversed.lplacement.lposition . to minZ)
   where minZ (P3 _ _ z) = Min z

-- All parts in a group are rotated around the part position of the
-- topmost group.
--
-- All parts not in a group are rotated around their own position
rotatePart :: Rotation -> PlacedPart -> PlacedPart
rotatePart r p = p & traversed %~ rotatePlaced r (partPosition p)

--------------------------------------------------------------------------------

rotateAroundPoint :: Int -> P3 -> P3 -> P3
rotateAroundPoint r (P3 x y z) =
   translate v . rotateZ r . translate (fmap negate v)
   where v = Vector3 x y z

--------------------------------------------------------------------------------
-- EXAMPLE

part :: Prim -> P3 -> Rotation -> Color -> PlacedPart
part prim pos rot col = Part $ Placed (Placement pos rot) col prim

examplePart = part (Brick 2 2) (P3 0 0 0) noRotation Red

exampleGroup =
   Group
   [ part (Brick 2 2) (P3 1 2 0) noRotation Green
   , part (Brick 3 2) (P3 4 3 0) noRotation Green
   , Group
     [ part (Brick 1 1) (P3 4 6 0) noRotation Tan
     , part (Brick 1 2) (P3 6 6 0) noRotation Tan
     ]
   ]

example :: [PlacedPart]
example =
   [ part (Brick 2 4) (P3 0 0 1) noRotation Red
   , part (Brick 1 2) (P3 0 5 1) noRotation Blue
   , exampleGroup
   ]
