{-# LANGUAGE TemplateHaskell #-}

module Editor where

import Model
import ModelRender (renderModel)
import Selector (Selector)
import Types
import qualified Selector

import Data.Foldable
import Data.Semigroup
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH

--------------------------------------------------------------------------------

data Editor
   = Place
     [PlacedPart]  -- ^ parts to place
     (Min Int)     -- ^ minimum z of parts to place
     [PlacedPart]  -- ^ already placed parts
   | Edit (Selector PlacedPart)
   deriving Show


makePlace :: [PlacedPart] -> [PlacedPart] -> Editor
makePlace toPlace placed = Place toPlace minZ placed
   where
      minZ = foldMap Model.getPartMinZ toPlace


lpartsToPlace :: Lens' Editor [PlacedPart]
lpartsToPlace = lens
   (\ed -> case ed of
         Place toPlace _ _ -> toPlace
         _                 -> [])
   (\ed toPlace -> case ed of
         Place _ _ placed -> makePlace toPlace placed
         _                -> ed)

lnonSelectedParts :: Lens' Editor [PlacedPart]
lnonSelectedParts = lens
   (\ed -> case ed of
         Place _ _ placed -> placed
         Edit s           -> s ^. Selector.nonSelected)
   (\ed placed -> case ed of
         Place toPlace z _ -> Place toPlace z placed
         Edit s            -> Edit (s & Selector.selected .~ placed))

lselectedParts :: Lens' Editor [PlacedPart]
lselectedParts = lens
   (\ed -> case ed of
         Place toPlace _ _ -> toPlace
         Edit s            -> s ^. Selector.selected)
   (\ed newSelected -> case ed of
         Place _ _ placed -> makePlace newSelected placed
         Edit s           -> Edit (s & Selector.selected .~ newSelected))

allParts :: Editor -> [PlacedPart]
allParts (Place toPlace _ placed) = toPlace ++ placed
allParts (Edit s)                 = toList s

--------------------------------------------------------------------------------

-- | Add new parts to place
placeNewParts :: [PlacedPart] -> Editor -> Editor
placeNewParts newParts ed = makePlace newParts (ed ^. lnonSelectedParts)

placeSelectedParts :: Editor -> Editor
placeSelectedParts ed = makePlace (ed ^. lselectedParts) (ed ^. lnonSelectedParts)

setSelectedPartsColor :: Color -> Editor -> Editor
setSelectedPartsColor c = lselectedParts.each.traversed.lcolor .~ c

cloneSelectedParts :: Editor -> Editor
cloneSelectedParts ed =
   makePlace (ed ^. lselectedParts) (ed ^. lselectedParts ++ ed ^. lnonSelectedParts)

groupSelectedParts :: Editor -> Editor
groupSelectedParts = lselectedParts %~ pure . Model.groupParts

ungroupSelectedParts :: Editor -> Editor
ungroupSelectedParts = lselectedParts %~ concatMap Model.ungroupPart

escapeEdit :: Editor -> Editor
escapeEdit (Place _ _ placed) = Edit $ Selector.makeSelector placed
escapeEdit (Edit s)           = Edit $ Selector.unselectAll s

--------------------------------------------------------------------------------

mapEdit f e@(Edit {}) = f e
mapEdit _ e           = e

mapPlace f e@(Place {}) = f e
mapPlace _ e            = e

--------------------------------------------------------------------------------
-- PLACE MODE FUNCTIONS

-- | Put parts to place into place
placeParts :: Editor -> Editor
placeParts = mapPlace $ \(Place toPlace _ placed) ->
   Edit $ Selector.makeSelector (toPlace ++ placed)

moveParts :: P3 -> Editor -> Editor
moveParts pos = mapPlace $ \(Place toPlace (Min z) placed) ->
   let (P3 x y _) = partPosition (head toPlace)
       refPos     = P3 x y z
       v          = vectorBetween refPos pos
       toPlace'   = map (translatePart v) toPlace
   in
      makePlace toPlace' placed

--------------------------------------------------------------------------------
-- EDIT MODE FUNCTIONS

unselectAll :: Editor -> Editor
unselectAll = mapEdit $ \(Edit s) -> Edit (Selector.unselectAll s)

toggleSelected :: Int -> Editor -> Editor
toggleSelected i = mapEdit $ \(Edit s) -> Edit (Selector.select i s)

deleteSelected :: Editor -> Editor
deleteSelected = mapEdit $ \(Edit s) -> Edit (s & Selector.selected .~ [])

--------------------------------------------------------------------------------

exampleEditor = makePlace [examplePart] []
