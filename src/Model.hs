{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Primitive
import Types

import Data.Foldable (traverse_)
import Lens.Micro

--------------------------------------------------------------------------------

newtype Rotation = Rotation Int deriving (Eq, Num, Ord, Show)

data Placement = Placement { position :: !P3, rotation :: !Rotation }
  deriving (Eq, Show)

lposition :: Lens' Placement P3
lposition = lens position (\p newPosition -> p { position = newPosition })

data Placed a = Placed !Placement !(Maybe Color) !a
  deriving Show

lplacement :: Lens' (Placed a) Placement
lplacement = lens (\(Placed p _ _) -> p) (\(Placed _ c a) p -> Placed p c a)

data Tree a = Part a
            | Group [Tree a]
  deriving (Foldable, Functor, Show, Traversable)

groupTrees :: [Tree a] -> Tree a
groupTrees = Group

ungroupTree :: Tree a -> [Tree a]
ungroupTree (Group ns) = ns
ungroupTree n          = [n]

--------------------------------------------------------------------------------

type PlacedPart = Tree (Placed Prim)

translatePart :: V3 Int -> PlacedPart -> PlacedPart
translatePart v = fmap (lplacement.lposition %~ translate v)

--------------------------------------------------------------------------------
-- EXAMPLE

part :: Prim -> P3 -> Rotation -> Maybe Color -> PlacedPart
part prim pos rot col = Part $ Placed (Placement pos rot) col prim

norot = 0

example :: [PlacedPart]
example =
   [ part (Brick 2 4) (P3 0 0 1) norot (Just Red)
   , part (Brick 1 2) (P3 0 5 1) norot (Just Blue)
   , Group
     [ part (Plate 6 10) (P3 0 0 0) norot (Just Green) ]
   ]
