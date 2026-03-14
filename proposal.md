# Text-based Game EDSL

In text-based adventure games, a player travels through a series of interconnected rooms, discovers items, and uncovers a story through exploration. Games can be completed by entering specific rooms, finding a particular item, or some combination of the two. We plan on implementing an EDSL in Haskell that allows users to define, validate, and run their own text-based adventure games.

## EDSL Interface

The EDSL will be built around the `WorldBuilder` monad which provides mechanism for accumuluating the world's specification as it is written. Below are the minimal set of our primitive operations which provide the necessary building blocks for creating a `World`.

```Haskell
item :: String -> WorldBuilder a
room :: String -> String -> [String] -> WorldBuilder a
path :: String -> String -> Direction -> WorldBuilder a
slide :: String -> String -> Direction -> WorldBuilder a
lockedPath :: String -> String -> Direction -> String -> WorldBuilder a
lockedSlide :: String -> String -> Direction -> String -> WorldBuilder a
endItems :: [String] -> String -> WorldBuilder a
endRoom :: String -> String -> WorldBuilder a
endRoomWithItems :: String -> [String] -> String -> WorldBuilder a
```

For example, `item` declares an item that can be found in the world, `room` declares a room in the world along with its description and any items it may contain, `path` declares a path between two rooms, etc. and the `end...` functions declare the end conditions necessary to complete a game in the World. While these are our primitive functions, we plan on creating derived functions that allow for easier implementation of a game.

```Haskell
buildWorld :: String -> [String] -> WorldBuilder a -> World a
buildDebugWorld :: String -> [String] -> WorldBuilder a -> World a
runWorld :: World a -> IO a
```

A `WorldBuilder` is run with the `buildWorld` function which takes the initial state of the world builder (The data needed for the start room) and creates a `World` from it, a representation of the game world. The function `buildDebugWorld` allows the user to build a World that might not yet be completable in order to test intermediate steps during the implementation. Finally, there is a `runWorld` function which is how a user can play the game through a textual interface.

## Underlying Types

At a high level, a `World` is a directed graph where each `Room` represents a node and a `Path` the connection between two nodes. This information together with necessary additional information, such as completing conditions, or existing items are stored in a `WorldSpec` data structure. The `WorldBuilder` monad manages this data structure as a State which the user constructs declaritively via the public interface.

```Haskell
newtype WorldBuilder a = WorldBuilder (State WorldSpec a)

data WorldSpec = WorldSpec
  { startRoom :: RoomId,
    specRooms :: Map RoomId RoomData,
    specItems :: [ItemId],
    specPaths :: [Path],
    specEndConditions :: Map EndCondition String
  }
```

A `World` must have an explicitly defined starting room, which is separate from the map of all other rooms. Each room and item has an ID and rooms have additional information, its description and available items, associated to it. A path represents a connection between two rooms, includes a direction and information about if an item is needed to unlock it. We implement 4 different directions, meaning that each room can have up to 4 neighboring rooms.

```Haskell
newtype RoomId = RoomId String deriving (Show, Read, Eq)
newtype ItemId = ItemId String deriving (Show, Read, Eq)

data RoomData = RoomData
  { roomDesc :: String
  , roomItems :: [ItemId]
  }

data Direction = North | South | East | West

data Path = Path
  { pathTo :: RoomId
  , pathFrom :: RoomId
  , pathDirection :: Direction
  , pathKey :: Maybe ItemId
  }
```

Lastly, we have the `EndCondition` type which is used to define conditions in order for a game to be completed. If a world has several `EndConditions` it suffices to fulfill one of them in order to complete the game. Such a condition can be related to the currently held items of a player, a room that needs to be entered, or both.

```Haskell
data EndCondition
  = HoldItems [ItemId]
  | EnterRoom RoomId
  | EnterRoomWith RoomId [ItemId]
  deriving (Eq, Show)
```

## Test Spec

Our test spec will primarily be based on being able to properly validate games throughout development. It's necessary for an author to be able to express invalid games and interact with them, but ultimately we want to be able to assist them with creating a valid, completable game. As such, if a WorldBuilder is successfully run through the `buildWorld` function, we guarantee that it will be a non-empty, completeable game world. If the game is played, there exists at least one path the player can take to reach some `EndCondition`. To do this, we will need to create a number of quick check generators that allow us to generate arbitrary `WorldBuilders` that construct both valid and invalid games, as well as develop a game solver which can be used to validate the `Worlds`.
