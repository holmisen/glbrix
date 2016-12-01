{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Selector
 ( Selector
 , makeSelector
 , selected
 , nonSelected
 , hasSelected
 , select
 , unselectAll
 )
where

import Data.Foldable (for_)
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH

data Selector a = Selector { _selected    :: [a]
                           , _nonSelected :: [a]
                           }
   deriving (Foldable, Functor, Show, Traversable)

makeLenses ''Selector

makeSelector elements = Selector { _selected = [], _nonSelected = elements }


hasSelected :: Selector a -> Bool
hasSelected = not . null . _selected


unselectAll :: Selector a -> Selector a
unselectAll s =
   s & nonSelected %~ (view selected s ++)
     & selected .~ []


-- | Selects/unselects part for given index.
select :: Int -> Selector a -> Selector a
select i s
   | i < 0 = s
   | i < n =
     moveElement i selected nonSelected s
   | otherwise =
     moveElement (i-n) nonSelected selected s
   where
      n = length (view selected s)


-- For some reason, I need Rank2Types for this.
moveElement :: Int -> Lens' (Selector a) [a] -> Lens' (Selector a) [a] -> Selector a -> Selector a
moveElement i from to s =
   s & from .~ rest
     & to %~ (p ++)
   where
      (p, rest) = pick i (view from s)


-- renderSelector :: Selector -> IO ()
-- renderSelector scene
--    | hasSelected scene = do
--         -- Render unselected
--         GL.polygonMode $= (GL.Line, GL.Line)
--         mapM_ renderModel (view nonSelected scene)

--         -- Render selected
--         GL.polygonMode $= (GL.Fill, GL.Fill)
--         mapM_ renderModel (view selectedParts scene)

--    | otherwise = do
--         mapM_ renderModel (allSelectorParts scene)

--------------------------------------------------------------------------------

pick :: Int -> [a] -> ([a],[a])
pick i xs = (x, before ++ after)
   where
      (before,r) = splitAt i xs
      (x, after) = splitAt 1 r
