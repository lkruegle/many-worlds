-- | This module exports the WorldBuilder api
--
-- The functions and types exported by this module enable users to declare
-- worlds made up of interconnected rooms, items, and obstacles.
module ManyWorlds.WorldBuilder
  ( WorldBuilder,

    -- * Data Types
    ItemId (),
    RoomId (),
    Direction (..),
    World (),

    -- * Declarative functions
    item,
    room,
    path,
    lockedPath,
    slide,
    lockedSlide,
    endItems,
    endRoom,
    endRoomWithItems,
    emptyRoom,

    -- * WorldBuilder Run functions
    buildWorld,
    buildWorld',

    -- * World solver
    solveWorld,
    SolveResult (..),
    StuckState (..),
  )
where

import Control.Monad.State
import qualified Data.Map as M
import Data.Text (Text)
import ManyWorlds.Internal
import ManyWorlds.InternalTypes
import ManyWorlds.WorldSolver

-- | Creates a playable World from the world builder.
--
-- This function returns both a built world and a result indicating whether the
-- world is solvable or not.
buildWorld ::
  -- | The WorldBuilder you want to build. The room it returns is considered
  -- the start room.
  WorldBuilder RoomId ->
  -- | A tuple containing the built world as well as a result indicating if the
  -- world is solvable or not.
  (World, SolveResult)
buildWorld builder = (world, solveWorld world)
  where
    world = buildWorld' builder

-- | Creates a playable World from the world builder.
--
-- The world returned by this function is the initial state of the world.
-- This builder does not enforce the properties of a winnable world.
buildWorld' ::
  -- | The WorldBuilder you want to build. The room it returns is considered
  -- the start room.
  WorldBuilder RoomId ->
  -- | A playable world with no guarantees of being valid
  World
buildWorld' builder = World spec initialState
  where
    initialState = PlayerState {currentRoom = startRoomId, heldItems = []}
    (startRoomId, spec) = runState builder emptySpec

-- | Declares a new item and adds it to the world.
item ::
  -- | The name of the item
  Text ->
  -- | Returns the id of the item
  WorldBuilder ItemId
item i = modify addItem >> return itm
  where
    itm = ItemId i
    addItem s = let itms = specItems s in s {specItems = itm : itms}

-- | Declares a new room and adds it to the world.
room ::
  -- | The name of the room
  Text ->
  -- | The textual description of the room, shown to players on entry
  Text ->
  -- | The ids of any items in the room
  [ItemId] ->
  -- | A monadic value containing the id of the created room
  WorldBuilder RoomId
room r d itms = modify addRoom >> return rmid
  where
    rmid = RoomId r
    rmdat = RoomData {roomDesc = d, roomItems = itms}
    addRoom s = s {specRooms = M.insert rmid rmdat $ specRooms s}

-- | Convenience declaration for a room with no items.
emptyRoom ::
  -- | The name of the room
  Text ->
  -- | The textual description of the room, shown to players on entry
  Text ->
  -- | A monadic value containing the id of the created room
  WorldBuilder RoomId
emptyRoom r d = room r d []

-- | Declares a bidirectional path with no item lock between two rooms.
--
-- This declares a path leading out of the "from" room in the given direction
-- to the "to" room, and implicitly adds a path from the "to" room to the "from"
-- room in the opposite direction. i.e.:
--
-- @
-- path from North to
-- @
--
-- creates two paths:
--  from --North--> to
--  to   --South--> from
--
-- In this case, both the explicit and implicit paths are unlocked
path ::
  -- | The "from" room id
  RoomId ->
  -- | The direction of the path from the "from" room
  Direction ->
  -- | The "to" room id
  RoomId ->
  WorldBuilder ()
path a d b = do
  addPath a d (Just b) Nothing
  addPath b (reverseDirection d) (Just a) Nothing

-- | Creates a bidirectional path locked by the given item between the two
-- rooms.
--
-- Behavior is the same as `path` except that both explicit and implicit paths
-- are locked.
lockedPath ::
  -- | The "from" room id
  RoomId ->
  -- | The direction of the path from the "from" room
  Direction ->
  -- | The "to" room id
  RoomId ->
  -- | The item that unlocks the path
  ItemId ->
  WorldBuilder ()
lockedPath a d b k = do
  addPath a d (Just b) (Just k)
  addPath b (reverseDirection d) (Just a) (Just k)

-- | Adds a unidirectional path, blocking the way back.
--
-- Similar to `path` except that the implicit path's destination is nothing.
-- This means that the player "sees" a path in that direction but is unable to
-- take it.
slide ::
  -- | The "from" room id
  RoomId ->
  -- | The direction of the path from the "from" room
  Direction ->
  -- | The "to" room id
  RoomId ->
  WorldBuilder ()
slide a d b = do
  addPath a d (Just b) Nothing
  addBlockedPath b (reverseDirection d)

-- | Adds a unidirectional path locked with an item, blocking the way back.
--
-- Same as `slide` but the explicit path is locked.
lockedSlide ::
  -- | The "from" room id
  RoomId ->
  -- | The direction of the path from the "from" room
  Direction ->
  -- | The "to" room id
  RoomId ->
  -- | The item that unlocks the slide
  ItemId ->
  WorldBuilder ()
lockedSlide a d b k = do
  addPath a d (Just b) (Just k)
  addBlockedPath b (reverseDirection d)

-- | Declares that holding the specified items is an end condition.
endItems ::
  -- | The items required to trigger this end condition
  [ItemId] ->
  -- | The text shown to the player when this condition is met
  Text ->
  WorldBuilder ()
endItems itms = addCondition (HoldItems itms)

-- | Declares that entering the specified room is an end condition.
endRoom ::
  -- | The room the player must enter to trigger this end condition
  RoomId ->
  -- | The text shown to the player when this condition is met
  Text ->
  WorldBuilder ()
endRoom rm = addCondition (EnterRoom rm)

-- | Specifies that entering the specified room with the specified items is an
-- end condition.
endRoomWithItems ::
  -- | The room the player must enter to trigger this end condition
  RoomId ->
  -- | The items required to trigger this end condition
  [ItemId] ->
  -- | The text shown to the player when this condition is met
  Text ->
  WorldBuilder ()
endRoomWithItems rm itms = addCondition (EnterRoomWith rm itms)

-- | Helper for adding end conditions to the world buidler.
addCondition :: EndCondition -> Text -> WorldBuilder ()
addCondition ec desc =
  modify
    (\s -> s {specEndConditions = M.insert ec desc (specEndConditions s)})

-- | Helper for adding a path to the world builder
addPath ::
  RoomId ->
  Direction ->
  Maybe RoomId ->
  Maybe ItemId ->
  WorldBuilder ()
addPath a d b k = modify addPath'
  where
    addPath' s =
      let newPath = Path {pathTo = b, pathKey = k}
          oldPaths = specPaths s
       in s {specPaths = M.insert (a, d) newPath oldPaths}

-- | Helper for adding a blocked path
addBlockedPath :: RoomId -> Direction -> WorldBuilder ()
addBlockedPath r d = addPath r d Nothing Nothing
