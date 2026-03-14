module ManyWorlds.InternalTypes where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Text (Text)

-- | The id of rooms in a World.
newtype RoomId = RoomId Text
  deriving (Show, Read, Eq, Ord)

-- | The id of items in a World.
newtype ItemId = ItemId Text
  deriving (Show, Read, Eq, Ord)

-- | Data type that describes the attributes of a room.
data RoomData = RoomData
  { -- | A description of the room, shown to the player when they view the room.
    roomDesc :: Text,
    -- | A list of items housed in the room that the player can pick up.
    roomItems :: [ItemId]
  }
  deriving (Read, Show, Eq, Ord)

-- | The directions a player can move between rooms.
data Direction = North | South | East | West
  deriving (Read, Show, Eq, Ord)

-- | Data type that describes a path between two rooms.
data Path = Path
  { pathTo :: Maybe RoomId,
    pathKey :: Maybe ItemId
  }
  deriving (Show, Read, Eq, Ord)

-- | Algebraic type that describes the various kinds of end conditions a game
-- can have.
data EndCondition
  = -- | Condition triggered by holding some set of items.
    HoldItems [ItemId]
  | -- | Condition triggered by entering a particular room.
    EnterRoom RoomId
  | -- | Condition triggered by entering a room while holding some set of items.
    EnterRoomWith RoomId [ItemId]
  deriving (Show, Read, Eq, Ord)

-- | Data type that describes the specification of a game world.
data WorldSpec = WorldSpec
  { -- | A map of room ids to their room data.
    specRooms :: Map RoomId RoomData,
    -- | A list of item ids that exist in the world
    specItems :: [ItemId],
    -- | A Map of paths between rooms that exist in the world.
    specPaths :: Map (RoomId, Direction) Path,
    -- | A map containing the end conditions and their descriptive text.
    specEndConditions :: Map EndCondition Text
  }
  deriving (Read, Show, Eq, Ord)

-- | The WorldBuilder monad. A Monad that provides users an interface for
-- declaratively building a game world.
type WorldBuilder a = State WorldSpec a

-- | Data type that describes the current state of a running game.
data PlayerState = PlayerState
  { -- | The room the player is currently in.
    currentRoom :: RoomId,
    -- | The list of items that the player currently holds.
    heldItems :: [ItemId]
  }
  deriving (Read, Show, Eq, Ord)

-- | The Actions a player can take while exploring a game
data Action
  = -- | Take a path leading out of the current room. The room must have a
    -- connecting path in the given direction.
    Move RoomId Direction
  | -- | Pick up an item in the current room.
    PickUp ItemId
  | -- | Replay the description of the current room.
    Look
  | -- | Check your inventory
    Inventory
  | -- | Only available once you've triggered an end condition. Ends the world.
    Quit
  | -- | Give the player reminders of what they can do.
    Help
  deriving (Read, Show)

-- | Describes the types of failures the game can describe when a player tries
-- to take an action but fails for some reason.
--
-- This includes feedback related to invalid input as well as game-logic related
-- obstacles.
data ActionFeedback
  = InvalidDirection Text
  | NoSuchPath Direction
  | PathLocked Direction ItemId
  | PathBlocked Direction
  | NoSuchItem Text
  | InvalidAction Text

-- TODO: I'd like to create a unified world state type that provides a simple
-- interface for querying the world state. this version works for now, but
-- probably causes some awkwardness when trying to run the game.

-- | A playable world.
--
-- A World contains the overall world spec as well as the current player state.
data World = World WorldSpec PlayerState
  deriving (Read, Show, Eq, Ord)

-- | Monad That provides a feedback mechanism.
--
-- This is intended for use when parsing player input and coercing it to
-- actions.
type FeedbackM a = ExceptT ActionFeedback (ReaderT World IO) a

-- | Monad that allows for read-only world operations. This ensures
-- user defined code for custom game text can't modify the game state
type WorldM a = ReaderT World IO a

-- | Interface for custom UI.
--
-- A default text interface is provided, but both feedback and action
-- descriptions can be overridden to custom functions.
data WorldConfig = WorldConfig
  { provideFeedback :: ActionFeedback -> WorldM Text,
    describeAction :: Action -> WorldM Text
  }

-- | Describes the ways a world can get stuck
data StuckState
  = -- | No outgoing paths exist in the given room
    NoPath RoomId
  | -- | This room contains at least one locked door and the probe reached the
    -- room without picking up necessary items.
    NeedItems RoomId [ItemId]
  deriving (Show, Read)

-- | Result of solving a world
data SolveResult
  = -- | The world is solvable, the provided actions are a valid solution to the
    -- world if taken in sequence
    Solvable [Action]
  | -- | The world has a solution but it is also possible to get stuck.
    Partial [Action] [StuckState]
  | -- | The world has no valid solution.
    Unsolvable [StuckState]
  deriving (Show, Read)
