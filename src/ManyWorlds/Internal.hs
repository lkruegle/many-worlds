{-# LANGUAGE TupleSections #-}

module ManyWorlds.Internal where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import ManyWorlds.InternalTypes

-- | Helper for creating an empty world spec
emptySpec :: WorldSpec
emptySpec =
  WorldSpec
    { specRooms = M.empty,
      specItems = [],
      specPaths = M.empty,
      specEndConditions = M.empty
    }

-- | Updates the world with the result of taking action.
--
-- This function assumes actions are well formed in the current world.
takeAction :: World -> Action -> Maybe World
takeAction world = \case
  Move _ direction -> Just $ takePath direction
  PickUp itm -> Just $ pickupItem itm
  Quit -> Nothing
  -- Remaining actions do not mutate the world
  _ -> Just world
  where
    (World spec state) = world
    pickupItem item =
      World spec (state {heldItems = item : heldItems state})
    takePath direction = World spec (state {currentRoom = newroom})
      where
        newroom = case pathTo (getPath world direction) of
          Just room -> room
          -- If the room is blocked, stay in the same room
          Nothing -> currentRoom state

-- | Helper for finding the reverse direction
reverseDirection :: Direction -> Direction
reverseDirection = \case
  North -> South
  South -> North
  East -> West
  West -> East

allDirections :: [Direction]
allDirections = [North, South, East, West]

-- The helpers below are pretty utilitarian. The World type was harder to
-- work with than I hoped so there's not much consistency of whether these
-- functions take full Worlds or their constituent parts. Probably worth
-- taking another look at the design of World in order to rewrite these
-- in a more consistent manner.

getPath :: World -> Direction -> Path
getPath (World spec state) direction =
  case M.lookup lookupkey (specPaths spec) of
    Just path -> path
    Nothing -> error $ "No path exists for " <> show lookupkey
  where
    lookupkey = (currentRoom state, direction)

currentRoomDesc :: World -> Text
currentRoomDesc world = roomDesc $ currentRoomData world

currentPaths :: World -> [(Direction, Path)]
currentPaths (World spec state) = mapMaybe lookupDir allDirections
  where
    room = currentRoom state
    paths = specPaths spec
    lookupDir dir = (dir,) <$> M.lookup (room, dir) paths

currentRoomData :: World -> RoomData
currentRoomData (World spec state) = case M.lookup roomid (specRooms spec) of
  Just roomdata -> roomdata
  Nothing -> error $ "No room data exists for " <> show roomid
  where
    roomid = currentRoom state

currentRoomItems :: World -> [ItemId]
currentRoomItems world =
  [ item
    | item <- roomItems $ currentRoomData world,
      item `notElem` inventory world
  ]

inventory :: World -> [ItemId]
inventory (World _ state) = heldItems state

checkCondition :: PlayerState -> EndCondition -> Bool
checkCondition state cond = case cond of
  HoldItems is -> allItemsHeld is
  EnterRoom r -> inRoom r
  EnterRoomWith r is -> inRoom r && allItemsHeld is
  where
    inRoom room = room == currentRoom state
    allItemsHeld items =
      let condItms = S.fromList items
          heldItms = S.fromList $ heldItems state
       in (condItms `S.intersection` heldItms) == condItms

-- | Checks all end conditions and returns the text associated with the
-- first one found that is satisfied.
checkEndConditions :: World -> Maybe Text
checkEndConditions (World spec state) =
  listToMaybe [text | (cond, text) <- conds, checkCondition state cond]
  where
    conds = M.toList (specEndConditions spec)
