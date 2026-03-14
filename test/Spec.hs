import Control.Monad.State (execState, put)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (pack)
import Generators
import ManyWorlds.Internal
import ManyWorlds.InternalTypes
import ManyWorlds.WorldBuilder
import Test.QuickCheck

-- ======================= --
-- Main --
-- ======================= --

-- | Test all QuickCheck properties
main :: IO ()
main = do
  quickCheck prop_addRoomToSpec
  quickCheck prop_addItemToSpec
  quickCheck prop_pathsUseExistingRooms
  quickCheck prop_correctPath
  quickCheck prop_correctLockedPath
  quickCheck prop_correctSlide
  quickCheck prop_correctLockedSlide
  quickCheck prop_staySolvable
  quickCheck prop_makeSolvable

-- ======================= --
-- API properties --
-- ======================= --

-- | Adding a room to a WorldSpec can behave in two ways:
--
-- - the given roomId already exists -> length of specRooms does not change
-- - the given roomId is new -> length of specRooms increases by 1
prop_addRoomToSpec :: Property
prop_addRoomToSpec = forAll worldSpecGen $ \(_, spec) -> do
  let txt = pack "test"
      before = Map.size (specRooms spec)
      spec' = execState (emptyRoom txt txt) spec
      after = Map.size (specRooms spec')
  if Map.member (RoomId txt) (specRooms spec)
    then before === after
    else (before + 1) === after

-- | Adding an item to a WorldSpec increases the length of specItems by 1
prop_addItemToSpec :: Property
prop_addItemToSpec = forAll worldSpecGen $ \(_, spec) -> do
  let txt = pack "test"
      before = length (specItems spec)
      spec' = execState (item txt) spec
      after = length (specItems spec')
  (before + 1) === after

-- | A new path can only connect rooms that exist in specRooms
--
-- Note: With the current generator this property is true by construction,
-- but could become an interesting property for possible extensions
prop_pathsUseExistingRooms :: Property
prop_pathsUseExistingRooms = forAll worldSpecGen $ \(_, spec) ->
  let existingRooms = Map.keys (specRooms spec)
      pathToRooms = [to | p <- Map.elems (specPaths spec), Just to <- [pathTo p]]
      pathFromRooms = [from | (from, _) <- Map.keys (specPaths spec)]
   in all (`elem` existingRooms) (pathToRooms ++ pathFromRooms)

-- | A new path adds a bidirectional connection between 2 rooms
-- that is not locked by any keys (items)
--
-- Uses worldSpecFilledGen to ensure reasonable test cases
prop_correctPath :: Property
prop_correctPath = forAll worldSpecFilledGen $ \(_, spec) ->
  forAll arbitrary $ \dir ->
    let roomIds = getRoomIds (specRooms spec)
        (from, to) = case roomIds of
          (x : y : _) -> (x, y)
          _ -> error "worldSpecFilledGen guarantees at least 2 rooms"
        spec' = execState (path from dir to) spec
     in case ( Map.lookup (from, dir) (specPaths spec'),
               Map.lookup (to, reverseDirection dir) (specPaths spec')
             ) of
          (Nothing, _) -> False
          (_, Nothing) -> False
          (Just p1, Just p2) ->
            pathTo p1 == Just to
              && isNothing (pathKey p1)
              && pathTo p2 == Just from
              && isNothing (pathKey p2)

-- | A new locked path adds a bidirectional connection between 2 rooms
-- that is locked by the same key (item) in both directions
--
-- Uses worldSpecFilledGen to ensure reasonable test cases
prop_correctLockedPath :: Property
prop_correctLockedPath = forAll worldSpecFilledGen $ \(_, spec) ->
  forAll arbitrary $ \dir ->
    let roomIds = getRoomIds (specRooms spec)
        (from, to) = case roomIds of
          (x : y : _) -> (x, y)
          _ -> error "worldSpecFilledGen guarantees at least 2 rooms"
        itm = case specItems spec of
          (x : _) -> x
          _ -> error "worldSpecFilledGen guarantees at least 1 item"
        spec' = execState (lockedPath from dir to itm) spec
     in case ( Map.lookup (from, dir) (specPaths spec'),
               Map.lookup (to, reverseDirection dir) (specPaths spec')
             ) of
          (Nothing, _) -> False
          (_, Nothing) -> False
          (Just p1, Just p2) ->
            pathTo p1 == Just to
              && pathKey p1 == Just itm
              && pathTo p2 == Just from
              && pathKey p2 == Just itm

-- | A new slide adds a unidirectional connection between 2 rooms
-- that is not locked by any keys (items).
-- The non-existing direction is indicated by a blocked path
--
-- Uses worldSpecFilledGen to ensure reasonable test cases
prop_correctSlide :: Property
prop_correctSlide = forAll worldSpecFilledGen $ \(_, spec) ->
  forAll arbitrary $ \dir ->
    let roomIds = getRoomIds (specRooms spec)
        (from, to) = case roomIds of
          (x : y : _) -> (x, y)
          _ -> error "worldSpecFilledGen guarantees at least 2 rooms"
        spec' = execState (slide from dir to) spec
     in case ( Map.lookup (from, dir) (specPaths spec'),
               Map.lookup (to, reverseDirection dir) (specPaths spec')
             ) of
          (Nothing, _) -> False
          (_, Nothing) -> False
          (Just p1, Just p2) ->
            pathTo p1 == Just to
              && isNothing (pathKey p1)
              && isNothing (pathTo p2)

-- | A new locked slide adds a unidirectional connection between 2 rooms
-- that is locked by a key (item).
-- The non-existing direction is indicated by a blocked path
--
-- Uses worldSpecFilledGen to ensure reasonable test cases
prop_correctLockedSlide :: Property
prop_correctLockedSlide = forAll worldSpecFilledGen $ \(_, spec) ->
  forAll arbitrary $ \dir ->
    let roomIds = getRoomIds (specRooms spec)
        (from, to) = case roomIds of
          (x : y : _) -> (x, y)
          _ -> error "worldSpecFilledGen guarantees at least 2 rooms"
        itm = case specItems spec of
          (x : _) -> x
          _ -> error "worldSpecFilledGen guarantees at least 1 item"
        spec' = execState (lockedSlide from dir to itm) spec
     in case ( Map.lookup (from, dir) (specPaths spec'),
               Map.lookup (to, reverseDirection dir) (specPaths spec')
             ) of
          (Nothing, _) -> False
          (_, Nothing) -> False
          (Just p1, Just p2) ->
            pathTo p1 == Just to
              && pathKey p1 == Just itm
              && isNothing (pathTo p2)

-- | Adding the following calls to API functions to a solvable WorldSpec
-- should not make it unsolvable:
--
-- - adding a new item (items for end conditions still have sam availablility)
-- - adding new end conditions (does not alter the fulfillable end conditions)
--
-- All other API functions can cause unsolvable WorldSpecs
-- (Note: existing rooms can be overwritten by new rooms with different items!)
prop_staySolvable :: Property
prop_staySolvable = forAll solvableWorldSpecGen $ \(start, spec) ->
  forAll (oneof (invariantApiGen spec)) $ \spec' ->
    let extended = do
          put spec'
          return start
     in case snd (buildWorld extended) of
          Unsolvable _ -> False
          _ -> True
  where
    invariantApiGen spec =
      concat
        [ [snd <$> itemGen spec],
          addIf
            (not (null (specItems spec)))
            [ snd <$> endItemsGen spec
            ],
          addIf
            (not (null (getRoomIds (specRooms spec))))
            [snd <$> endRoomGen spec],
          addIf
            (not (null (getRoomIds (specRooms spec))) && not (null (specItems spec)))
            [snd <$> endRoomWithItemsGen spec]
        ]

-- | Given an unsolvable WorldSpec that includes any end conditions
-- there is always a way to make it solvable by making any one of
-- the existings end conditions fulfillable by:
--
-- - adding path from start to an end room
-- - adding all necessary items to the start room
--
-- The solver must be able to  find that trivially added solution.
prop_makeSolvable :: Property
prop_makeSolvable = forAll unsolvableWorldSpecGen $ \(start@(RoomId startId), spec) ->
  let endCond = case Map.keys (specEndConditions spec) of
        (x : _) -> x
        [] -> error "unsolvableWorldSpecGen guarantuees at least 1 EndCondition"
   in case endCond of
        EnterRoom end ->
          let extended = do
                put spec
                path start North end
                return start
           in case snd (buildWorld extended) of
                Unsolvable _ -> False
                _ -> True
        HoldItems itms ->
          let extended = do
                put spec
                room startId (pack "start") itms
           in case snd (buildWorld extended) of
                Unsolvable _ -> False
                _ -> True
        EnterRoomWith end itms ->
          let extended = do
                put spec
                path start North end
                room startId (pack "start") itms
           in case snd (buildWorld extended) of
                Unsolvable _ -> False
                _ -> True
