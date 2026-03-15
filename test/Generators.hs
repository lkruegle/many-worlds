module Generators where

import Control.Monad (void)
import Control.Monad.State (execState, put, runState)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import ManyWorlds.Internal
import ManyWorlds.InternalTypes
import ManyWorlds.WorldBuilder
import Test.QuickCheck

-- ======================= --
-- Helper functions --
-- ======================= --

-- | Generator for arbitrary Text
genText :: Gen Text
genText = pack <$> arbitrary

-- | Make directions an instance of Arbitrary to use in prop testing
instance Arbitrary Direction where
  arbitrary = elements [North, South, East, West]

-- | Helper function to get a list of available room ids in a WorldSpec
getRoomIds :: Map.Map RoomId RoomData -> [RoomId]
getRoomIds = Map.keys

-- | Helper functions that returns the given list if given condtion is met
-- otherwise the empty list is returned
addIf :: Bool -> [a] -> [a]
addIf cond ls = if cond then ls else []

-- ======================= --
-- API generators --
-- ======================= --

-- | Type alias for functions that change a WorldSpec
-- according to the result of an API function call
--
-- The returned Generator always returns a WorldBuilder with a unit type
-- to generalize the different return types of the API functions.
-- To store the actual result the modified WorldSpec is also returned.
type ApiGen = WorldSpec -> Gen (WorldBuilder (), WorldSpec)

-- | Generator to add an Item to a WorldSpec
--
-- Calls the API function `item` with an arbitrary id text
-- The item might be a dupiicate of a previous
itemGen :: ApiGen
itemGen spec = do
  name <- genText
  let wb = void (item name)
  return (wb, execState wb spec)

-- | Generator to add an empty room to a WorldSpec
--
-- Calls the API function `emptyRoom` with an arbitrary id text
emptyRoomGen :: ApiGen
emptyRoomGen spec = do
  name <- genText
  let wb = void (emptyRoom name (pack ""))
  return (wb, execState wb spec)

-- | Generator to add a room containing items to a WorldSpec
--
-- Calls the API function `room` with an arbitrary id text and
-- an arbitrary sublist of items available in the given WorldSpec.
roomGen :: ApiGen
roomGen spec = do
  name <- genText
  itms <- sublistOf (specItems spec)
  let wb = void (room name (pack "") itms)
  return (wb, execState wb spec)

-- | Helper generator that retirns 2 different random room ids that are
-- available in the given WorldSpec and an arbitrary Direction.
--
-- Handles the common parameters need for path types
genBasicPath :: WorldSpec -> Gen (RoomId, Direction, RoomId)
genBasicPath spec = do
  let rooms = getRoomIds (specRooms spec)
  from <- elements rooms
  dir <- arbitrary
  to <- elements (filter (/= from) rooms)
  return (from, dir, to)

-- | Generator to add an open path to a WorldSpec
--
-- Calls the API function `path` and uses genBasicPath
pathGen :: ApiGen
pathGen spec = do
  (from, dir, to) <- genBasicPath spec
  let wb = path from dir to
  return (wb, execState wb spec)

-- | Generator to add an open slide to a WorldSpec
--
-- Calls the API function `slide` and uses genBasicPath
slideGen :: ApiGen
slideGen spec = do
  (from, dir, to) <- genBasicPath spec
  let wb = slide from dir to
  return (wb, execState wb spec)

-- | Generator to add a path that is locked by a key (item) to a WorldSpec
--
-- Calls the API function `lockedPath` and uses genBasicPath
-- and chooses random element from the available item list the WorldSpec.
lockedPathGen :: ApiGen
lockedPathGen spec = do
  (from, dir, to) <- genBasicPath spec
  itm <- elements (specItems spec)
  let wb = lockedPath from dir to itm
  return (wb, execState wb spec)

-- | Generator to add a path that is locked by a key (item) to a WorldSpec
--
-- Calls the API function `lockedSlide` and uses genBasicPath
-- and chooses random element from the available item list the WorldSpec.
lockedSlideGen :: ApiGen
lockedSlideGen spec = do
  (from, dir, to) <- genBasicPath spec
  itm <- elements (specItems spec)
  let wb = lockedSlide from dir to itm
  return (wb, execState wb spec)

-- | Generator to add a list of items as an end condition
--
-- Calls the API function `endItems` and gives it an arbitrary
-- sublist of the items available in the given WorldSpec
endItemsGen :: ApiGen
endItemsGen spec = do
  items <- sublistOf (specItems spec)
  let wb = endItems items (pack "")
  return (wb, execState wb spec)

-- | Generator to add a room as an end condition
--
-- Calls the API function `endRoom` and gives it an arbitrary
-- room id available in the given WorldSpec
endRoomGen :: ApiGen
endRoomGen spec = do
  end <- elements (getRoomIds (specRooms spec))
  let wb = endRoom end (pack "")
  return (wb, execState wb spec)

-- | Generator to add a room and a list of items as an end condition
--
-- Calls the API function `endRoomWithItems` and gives it an arbitrary room id
-- and an arbitrary sublist of the items available in the given WorldSpec
endRoomWithItemsGen :: ApiGen
endRoomWithItemsGen spec = do
  end <- elements (getRoomIds (specRooms spec))
  items <- sublistOf (specItems spec)
  let wb = endRoomWithItems end items (pack "")
  return (wb, execState wb spec)

-- | Generate an API function with one of the above defined Generators
--
-- This generator checks if enough room or items ids are available in
-- the given WorldSpec to use a generator
apiFunctionGen :: ApiGen
apiFunctionGen spec = frequency $ independent ++ dependent
  where
    independent =
      -- no conditions need to be met to se these functions
      [ (3, itemGen spec),
        (1, emptyRoomGen spec)
      ]
    dependent =
      concat
        [ addIf
            (not (null (specItems spec)))
            -- if at least 1 item exists
            [ (3, roomGen spec),
              (1, endItemsGen spec)
            ],
          addIf
            (length (getRoomIds (specRooms spec)) > 1)
            -- if at least 2 rooms exist
            [ (2, pathGen spec),
              (2, slideGen spec)
            ],
          addIf
            (not (null (getRoomIds (specRooms spec))))
            -- if at least 1 room exists
            [ (1, endRoomGen spec)
            ],
          addIf
            (not (null (getRoomIds (specRooms spec))) && not (null (specItems spec)))
            -- if at least 1 room and 1 item exist
            [ (1, endRoomWithItemsGen spec)
            ],
          addIf
            (length (getRoomIds (specRooms spec)) > 1 && not (null (specItems spec)))
            -- if at least 2 rooms and 1 item exist
            [ (2, lockedPathGen spec),
              (2, lockedSlideGen spec)
            ]
        ]

-- | Generate a return call returning the id of a start room
--
-- Converts a "WorldBuilder ()" into the "WorldBuilder RoomId" which can
-- be used for building a World
-- Makes sure that at least a start room is present
returnGen :: WorldBuilder () -> WorldSpec -> Gen (WorldBuilder RoomId)
returnGen wb spec = case getRoomIds (specRooms spec) of
  -- if there are no rooms in the given WorldSpec add a start room
  [] -> do
    name <- genText
    items <- sublistOf (specItems spec) -- possibly empty list
    return $ wb >> room name (pack "") items
  -- if the given WorldSpec has rooms, choose one of them as start room
  rooms -> do
    startRoom <- elements rooms
    return $ wb >> return startRoom

-- ======================= --
-- WorldBuilder generator --
-- ======================= --

-- | Generate an arbitrary WorldBuilder
--
-- The WorldBuilder will at least include a start room
-- "depths" decides how many API functions are called
worldBuilderGen :: Gen (WorldBuilder RoomId)
worldBuilderGen = sized $ \depth -> nestFunctions depth (return ()) emptySpec
  where
    nestFunctions :: Int -> WorldBuilder () -> WorldSpec -> Gen (WorldBuilder RoomId)
    nestFunctions 0 wb spec = returnGen wb spec
    nestFunctions d wb spec = do
      (wb', spec') <- apiFunctionGen spec
      nestFunctions (d - 1) (wb >> wb') spec'

-- | Generates a WorldSpec and a RoomId representing a start room
-- that is guaranteed to be (partially) solvable when given to buildWorld
--
-- Uses `resize` to avoid explosions in size of the generated WorldSpecs
solvableWorldSpecGen :: Gen (RoomId, WorldSpec)
solvableWorldSpecGen =
  resize 20 worldSpecGen `suchThat` \(startRoom, spec) ->
    case snd (buildWorld (put spec >> return startRoom)) of
      Unsolvable _ -> False
      _ -> True

-- | Generates a WorldSpec and a RoomId representing a start room
-- that is guaranteed to be unsolvable when given to buildWorld
--
-- Uses `resize` to avoid explosions in size of the generated WorldSpecs
unsolvableWorldSpecGen :: Gen (RoomId, WorldSpec)
unsolvableWorldSpecGen =
  resize 20 worldSpecGen `suchThat` \(startRoom, spec) ->
    case snd (buildWorld (put spec >> return startRoom)) of
      Unsolvable _ -> not (null (specEndConditions spec)) -- unsolvable but with EndConditions
      _ -> False

-- | Generates a WorldSpec with at least 2 rooms and 1 item and
-- a RoomId representing a start room
worldSpecFilledGen :: Gen (RoomId, WorldSpec)
worldSpecFilledGen =
  worldSpecGen `suchThat` \(startRoom, spec) ->
    length (getRoomIds (specRooms spec)) > 1 && not (null (specItems spec))

-- | Generates a WorldSpec and a RoomId representing a start room
--
-- The type is not an actual WorldBuilder to be able to use it
-- with QuickCheck's "forAll", which needs the return type to be an instance of Show
--
-- This Generator is used by all other WorldSpec Generators
-- in combination with QuickCheck's `suchThat` to filter for specific WorldSpecs
worldSpecGen :: Gen (RoomId, WorldSpec)
worldSpecGen = (\wb -> runState wb emptySpec) <$> worldBuilderGen

{- Generated Example (with readable RoomIds):
  World (
    WorldSpec {
      specRooms = fromList [
        (RoomId "1",RoomData {roomDesc = "_\1064511", roomItems = []}),
        (RoomId "2",RoomData {roomDesc = "`@", roomItems = [ItemId "U1",ItemId "'\1003023",ItemId "l="]}),
        (RoomId "3",RoomData {roomDesc = "\ETBr", roomItems = [ItemId "\1104189\&3",ItemId ":,",ItemId "^^",ItemId "}\1072599",ItemId "'\1003023",ItemId "l="]}),
        (RoomId "4",RoomData {roomDesc = "\50424\140392", roomItems = [ItemId "U1"]})],
      specItems = [ItemId "\RS)",ItemId "\1104189\&3",ItemId ":,",ItemId "^^",ItemId "U1",ItemId "}\1072599",ItemId "fi",ItemId "'\1003023",ItemId "l="],
      specPaths = fromList [
        ((RoomId "1",North),Path {pathTo = Just (RoomId "2"), pathKey = Just (ItemId "U1")}),
        ((RoomId "1",East),Path {pathTo = Just (RoomId "4"), pathKey = Just (ItemId "^^")}),
        ((RoomId "1",West),Path {pathTo = Just (RoomId "2"), pathKey = Nothing}),
        ((RoomId "2",North),Path {pathTo = Just (RoomId "3"), pathKey = Nothing}),
        ((RoomId "2",South),Path {pathTo = Just (RoomId "1"), pathKey = Just (ItemId "U1")}),
        ((RoomId "2",East),Path {pathTo = Nothing, pathKey = Nothing}),
        ((RoomId "2",West),Path {pathTo = Nothing, pathKey = Nothing}),
        ((RoomId "3",South),Path {pathTo = Just (RoomId "2"), pathKey = Nothing}),
        ((RoomId "3",East),Path {pathTo = Just (RoomId "1"), pathKey = Just (ItemId "^^")}),
        ((RoomId "4",North),Path {pathTo = Just (RoomId "2"), pathKey = Just (ItemId "}\1072599")}),
        ((RoomId "4",West),Path {pathTo = Just (RoomId "1"), pathKey = Just (ItemId "^^")})],
      specEndConditions = fromList [
        (HoldItems [ItemId "'\1003023",ItemId "l="],"\202366\26726"),
        (HoldItems [ItemId ":,",ItemId "^^",ItemId "U1",ItemId "fi",ItemId "l="],"]\1018790"),
        (EnterRoom (RoomId "2"),":U"),
        (EnterRoom (RoomId "4"),"u\DC1")]})
    (PlayerState {currentRoom = RoomId "2", heldItems = []})
-}
