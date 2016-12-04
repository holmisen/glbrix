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
   = Pick (Selector PlacedPart)
   | Place
     [PlacedPart]  -- ^ parts to place
     (Min Int)     -- ^ minimum z of parts to place
     (Selector PlacedPart)  -- ^ already placed parts
   deriving Show


makeEditor :: [PlacedPart] -> Editor
makeEditor parts = Pick (Selector.makeSelector parts)

makePlace :: [PlacedPart] -> Selector PlacedPart -> Editor
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
         Place _ _ s -> s ^. Selector.nonSelected
         Pick s      -> s ^. Selector.nonSelected)
   (\ed placed -> case ed of
         Place toPlace z _ -> Place toPlace z (Selector.makeSelector placed)
         Pick s            -> Pick (s & Selector.selected .~ placed))

lselectedParts :: Lens' Editor [PlacedPart]
lselectedParts = lens
   (\ed -> case ed of
         Place toPlace _ _ -> toPlace
         Pick s            -> s ^. Selector.selected)
   (\ed newSelected -> case ed of
         Place _ _ placed -> makePlace newSelected placed
         Pick s           -> Pick (s & Selector.selected .~ newSelected))

allParts :: Editor -> [PlacedPart]
allParts ed = ed ^. lselectedParts ++ ed ^. lnonSelectedParts

--------------------------------------------------------------------------------

-- | Add new parts to place
placeNewParts :: [PlacedPart] -> Editor -> Editor
placeNewParts newParts (Pick s)      = makePlace newParts s
placeNewParts newParts (Place _ _ s) = makePlace newParts s

placeNewParts newParts ed = makePlace newParts (Selector.makeSelector $ allParts ed)

placeSelectedParts :: Editor -> Editor
placeSelectedParts (Pick s) = makePlace (s ^. Selector.selected) s
placeSelectedParts ed       = ed

setSelectedPartsColor :: Color -> Editor -> Editor
setSelectedPartsColor c = lselectedParts.each.traversed.lcolor .~ c

cloneSelectedParts :: Editor -> Editor
cloneSelectedParts ed =
   makePlace (ed ^. lselectedParts) (Selector.makeSelector $ allParts ed)

groupSelectedParts :: Editor -> Editor
groupSelectedParts = lselectedParts %~ pure . Model.groupParts

ungroupSelectedParts :: Editor -> Editor
ungroupSelectedParts = lselectedParts %~ concatMap Model.ungroupPart

moveSelectedParts :: V3 Int -> Editor -> Editor
moveSelectedParts v = lselectedParts.each %~ translatePart v

rotateSelectedParts :: Editor -> Editor
rotateSelectedParts = id -- TODO

escapeEdit :: Editor -> Editor
escapeEdit (Place _ _ s) = Pick $ s
escapeEdit (Pick s)      = Pick $ Selector.unselectAll s

--------------------------------------------------------------------------------

mapPick f e@(Pick {}) = f e
mapPick _ e           = e

mapPlace f e@(Place {}) = f e
mapPlace _ e            = e

--------------------------------------------------------------------------------
-- PLACE MODE FUNCTIONS

-- | Put parts to place into place
placeParts :: Editor -> Editor
placeParts ed = Pick $ Selector.makeSelector (allParts ed)
   -- mapPlace $ \(Place toPlace _ placed) ->
   -- Pick $ Selector.makeSelector (toPlace ++ placed)

moveParts :: P3 -> Editor -> Editor
moveParts pos = mapPlace $ \(Place toPlace (Min z) placed) ->
   let
      (P3 x y _) = partPosition (head toPlace)
      refPos     = P3 x y z
      v          = vectorBetween refPos pos
      toPlace'   = map (translatePart v) toPlace
   in
      makePlace toPlace' placed

--------------------------------------------------------------------------------
-- EDIT MODE FUNCTIONS

unselectAll :: Editor -> Editor
unselectAll = mapPick $ \(Pick s) -> Pick (Selector.unselectAll s)

toggleSelected :: Int -> Editor -> Editor
toggleSelected i = mapPick $ \(Pick s) -> Pick (Selector.select i s)

deleteSelected :: Editor -> Editor
deleteSelected = mapPick $ \(Pick s) -> Pick (s & Selector.selected .~ [])

--------------------------------------------------------------------------------

exampleEditor :: Editor
exampleEditor = makePlace [examplePart] (Selector.makeSelector [])
