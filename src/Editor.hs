{-# LANGUAGE TemplateHaskell #-}

module Editor where

import Model
import ModelRender (renderModel)
import Selector (Selector)
import Types
import qualified Selector

import Data.Foldable
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH

--------------------------------------------------------------------------------

data Editor
   = Place [PlacedPart] [PlacedPart]
   | Edit (Selector PlacedPart)
   deriving Show


lpartsToPlace :: Lens' Editor [PlacedPart]
lpartsToPlace = lens
   (\ed -> case ed of
         Place toPlace placed -> toPlace
         _                    -> [])
   (\ed toPlace -> case ed of
         Place _ placed -> Place toPlace placed
         _              -> ed)

lnonSelectedParts :: Lens' Editor [PlacedPart]
lnonSelectedParts = lens
   (\ed -> case ed of
         Place _ placed -> placed
         Edit s         -> s ^. Selector.nonSelected)
   (\ed placed -> case ed of
         Place toPlace _ -> Place toPlace placed
         Edit s          -> Edit (s & Selector.selected .~ placed))

lselectedParts :: Lens' Editor [PlacedPart]
lselectedParts = lens
   (\ed -> case ed of
         Place toPlace placed -> toPlace
         Edit s               -> s ^. Selector.selected)
   (\ed newSelected -> case ed of
         Place _ placed -> Place newSelected placed
         Edit s         -> Edit (s & Selector.selected .~ newSelected))

allParts :: Editor -> [PlacedPart]
allParts (Place toPlace placed) = toPlace ++ placed
allParts (Edit s)               = toList s

--------------------------------------------------------------------------------

-- | Add new parts to place
placeNewParts :: [PlacedPart] -> Editor -> Editor
placeNewParts toPlace (Place _ placed) = Place toPlace placed
placeNewParts toPlace (Edit placed)   = Place toPlace $ toList placed

placeSelectedParts :: Editor -> Editor
placeSelectedParts ed = Place (ed ^. lselectedParts) (ed ^. lnonSelectedParts)

setSelectedPartsColor :: Color -> Editor -> Editor
setSelectedPartsColor c = lselectedParts.each.traversed.lcolor .~ c

cloneSelectedParts :: Editor -> Editor
cloneSelectedParts ed =
   Place (ed ^. lselectedParts) (ed ^. lselectedParts ++ ed ^. lnonSelectedParts)

groupSelectedParts :: Editor -> Editor
groupSelectedParts = lselectedParts %~ pure . Model.groupParts

ungroupSelectedParts :: Editor -> Editor
ungroupSelectedParts = lselectedParts %~ concatMap Model.ungroupPart

escapeEdit :: Editor -> Editor
escapeEdit (Place _ placed) = Edit $ Selector.makeSelector placed
escapeEdit (Edit s)         = Edit $ Selector.unselectAll s

--------------------------------------------------------------------------------

mapEdit f e@(Edit {}) = f e
mapEdit _ e           = e

mapPlace f e@(Place {}) = f e
mapPlace _ e            = e

--------------------------------------------------------------------------------
-- PLACE MODE FUNCTIONS

-- | Put parts to place into place
placeParts :: Editor -> Editor
placeParts = mapPlace $ \(Place toPlace placed) ->
   Edit $ Selector.makeSelector (toPlace ++ placed)

moveParts :: P3 -> Editor -> Editor
moveParts pos = mapPlace $ \(Place toPlace placed) ->
   let refPos   = partPosition (head toPlace)
       v        = vectorBetween refPos pos
       toPlace' = map (translatePart v) toPlace
   in
      Place toPlace' placed

--------------------------------------------------------------------------------
-- EDIT MODE FUNCTIONS

unselectAll :: Editor -> Editor
unselectAll = mapEdit $ \(Edit s) -> Edit (Selector.unselectAll s)

toggleSelected :: Int -> Editor -> Editor
toggleSelected i = mapEdit $ \(Edit s) -> Edit (Selector.select i s)

deleteSelected :: Editor -> Editor
deleteSelected = mapEdit $ \(Edit s) -> Edit (s & Selector.selected .~ [])

--------------------------------------------------------------------------------

exampleEditor = Place [examplePart] []
