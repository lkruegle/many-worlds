import Test.QuickCheck

import Control.Monad.State (State, execState)
import Control.Monad (void)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)

import ManyWorlds.Types
import ManyWorlds.WorldBuilder
import ManyWorlds.WorldRunner
import ManyWorlds.Internal

-- ======================= --
-- Main --
-- ======================= --

main :: IO ()
main = do
  quickCheck prop_addRoomToSpec
  quickCheck prop_addItemToSpec
  quickCheck prop_pathsUseExistingRooms
  quickCheck prop_correctPath
  quickCheck prop_correctLockedPath
  quickCheck prop_correctSlide
  quickCheck prop_correctLockedSlide

-- ======================= --
-- Helper functions --
-- ======================= --

genText :: Gen Text
genText = pack <$> arbitrary

instance Arbitrary Direction where
  arbitrary = elements [North, South, East, West] 

getRoomIds :: Map.Map RoomId RoomData -> [RoomId]
getRoomIds = Map.keys

-- ======================= --
-- API generators --
-- ======================= --

type ApiGen = WorldSpec -> Gen (WorldBuilder (), WorldSpec)

-- Generators independent of previous Generators

itemGen :: ApiGen
itemGen spec = do 
  name <- genText
  let wb = void (item name)
  return (wb, execState wb spec)

emptyRoomGen :: ApiGen
emptyRoomGen spec = do
  name <- genText
  desc <- genText
  let wb = void (emptyRoom name desc)
  return (wb, execState wb spec)

-- Generator dependent on existing RoomIds and ItemIds

roomGen :: ApiGen
roomGen spec = do
  name <- genText
  desc <- genText
  items <- sublistOf (specItems spec)
  let wb = void (room name desc items)
  return (wb, execState wb spec)

genBasicPath :: WorldSpec -> Gen (RoomId, Direction, RoomId)
genBasicPath spec = do
  let rooms = getRoomIds (specRooms spec)
  from <- elements rooms
  dir  <- arbitrary
  to   <- elements (filter (/= from) rooms)
  return (from, dir, to)

pathGen :: ApiGen
pathGen spec = do
  (from, dir, to) <- genBasicPath spec
  let wb = path from dir to
  return (wb, execState wb spec)

slideGen :: ApiGen
slideGen spec = do
  (from, dir, to) <- genBasicPath spec
  let wb = slide from dir to
  return (wb, execState wb spec)

lockedPathGen :: ApiGen
lockedPathGen spec = do
  (from, dir, to) <- genBasicPath spec
  itm <- elements (specItems spec)
  let wb = lockedPath from dir to itm
  return (wb, execState wb spec)

lockedSlideGen :: ApiGen
lockedSlideGen spec = do
  (from, dir, to) <- genBasicPath spec
  itm <- elements (specItems spec)
  let wb = lockedSlide from dir to itm
  return (wb, execState wb spec)

endItemsGen :: ApiGen
endItemsGen spec = do
  items <- sublistOf (specItems spec)
  desc <- genText
  let wb = endItems items desc
  return (wb, execState wb spec)

endRoomGen :: ApiGen
endRoomGen spec = do
  end <- elements (getRoomIds (specRooms spec))
  desc <- genText
  let wb = endRoom end desc
  return (wb, execState wb spec)

endRoomWithItemsGen :: ApiGen
endRoomWithItemsGen spec = do
  end <- elements (getRoomIds (specRooms spec))
  items <- sublistOf (specItems spec)
  desc <- genText
  let wb = endRoomWithItems end items desc
  return (wb, execState wb spec)

-- Generator for random API function that is possible to execute with current WorldSpec

-- | Generate one API function
apiFunctionGen :: ApiGen
apiFunctionGen spec = frequency $ independent ++ dependent
  where
    independent = 
      [ (3, itemGen spec)
      , (1, emptyRoomGen spec)
      ]
    dependent = concat 
      [ addIf (not (null (specItems spec))) -- if items exist
              [ (3, roomGen spec)
              , (1, endItemsGen spec)
              ],
        addIf (length (getRoomIds (specRooms spec)) > 1) -- if at least 2 rooms exist
              [ (2, pathGen spec)
              , (2, slideGen spec)
              ],
        addIf (not (null (getRoomIds (specRooms spec)))) -- if rooms exist
              [(1, endRoomGen spec) 
              ],
        addIf (not (null (getRoomIds (specRooms spec))) && not (null (specItems spec))) -- rooms and items exist
              [(1, endRoomWithItemsGen spec)
              ],
        addIf (length (getRoomIds (specRooms spec)) > 1 && not (null (specItems spec))) -- 2 rooms and items exist
              [ (2, lockedPathGen spec)
              , (2, lockedSlideGen spec)
              ]
      ]
    addIf :: Bool -> [a] -> [a]
    addIf cond ls = if cond then ls else []

returnGen :: WorldBuilder () -> WorldSpec -> Gen (WorldBuilder RoomId)
returnGen wb spec = case getRoomIds (specRooms spec) of
  [] -> do
    name <- genText
    desc <- genText
    items <- sublistOf (specItems spec) -- possibly empty list
    return $ wb >> room name desc items
  rooms -> do
    startRoom <- elements rooms
    return $ wb >> return startRoom

-- ======================= --
-- WorldBuilder generator --
-- ======================= --

-- | Generate an arbitrary WorldBuilder (completion not guarantueed)
worldBuilderGen :: Gen (WorldBuilder RoomId)
worldBuilderGen = sized $ \depth -> nestFunctions depth (return ()) emptySpec
  where
  nestFunctions :: Int -> WorldBuilder () -> WorldSpec -> Gen (WorldBuilder RoomId)
  nestFunctions 0 wb spec = returnGen wb spec
  nestFunctions d wb spec = do
    (wb', spec') <- apiFunctionGen spec
    nestFunctions (d-1) (wb >> wb') spec'

-- | Generate a WorldSpec with at least 2 rooms and 1 item
worldSpecFilledGen :: Gen WorldSpec
worldSpecFilledGen = worldSpecGen `suchThat` \spec -> 
  length (getRoomIds (specRooms spec)) > 1 && length (specItems spec) > 0

worldSpecGen :: Gen WorldSpec
worldSpecGen = (\wb -> execState wb emptySpec) <$> worldBuilderGen

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

-- ======================= --
-- API properties --
-- ======================= --

{- Checking behavior of API functions:

room / emptyRoom:
  - increases (length specRooms) by one if name does not exist yet
  - does not increase length if name already exists

item: 
  - increases (length specItems) by one if name does not exist yet
  - does not increase length if name already exists

all paths/slides:
  - to and from are existing RoomIds

path:
  - (from - to) and (to - from) exist

slide:
  - (from - to) exists and (to - from) does NOT exist

lockedPath:
  - (from - to) and (to - from) exist
  - (from - to) and (to - from) have same key
  - key is existing ItemId

lockedSlide:
  - (from - to) exists and (to - from) does NOT exist
  - key is existing ItemId

endItems

Cannot invalidate World:
  - room / emptyRoom
  - item
  - path
  - any EndCondition
  
Should invalidate World:
  - appending new room
-} 

-- | Check whether Map size of specRooms increases correctly 
prop_addRoomToSpec :: Property
prop_addRoomToSpec = forAll worldSpecGen $ \spec -> do
  let txt = pack "test"
      before = Map.size (specRooms spec)
      spec' = execState (emptyRoom txt txt) spec
      after = Map.size (specRooms spec')
  if Map.member (RoomId txt) (specRooms spec)
    then before === after
    else (before + 1) === after

-- | Check whether length of specItems increases correctly
prop_addItemToSpec :: Property
prop_addItemToSpec = forAll worldSpecGen $ \spec -> do
  let txt = pack "test"
      before = length (specItems spec)
      spec' = execState (item txt) spec
      after = length (specItems spec')
  if ItemId txt `elem` specItems spec
    then before === after
    else (before + 1) === after

-- | Check that all paths use existing RoomIds (TODO by construction in Generator?)
prop_pathsUseExistingRooms :: Property
prop_pathsUseExistingRooms = forAll worldSpecGen $ \spec ->
  let existingRooms = Map.keys (specRooms spec)
      pathToRooms = [ to | p <- Map.elems (specPaths spec), Just to <- [pathTo p]]
      pathFromRooms = [ from | (from, _) <- Map.keys (specPaths spec)]
  in all (`elem` existingRooms) (pathToRooms ++ pathFromRooms)

-- | Check that a path adds correct open connections
prop_correctPath :: Property
prop_correctPath = forAll worldSpecFilledGen $ \spec ->
  forAll arbitrary $ \dir ->
    let roomIds = getRoomIds (specRooms spec)
        (from, to) = case roomIds of
                      (x:y:_) -> (x,y)
                      _ -> error "worldSpecFilledGen guarantees at least 2 rooms"
        spec' = execState (path from dir to) spec
    in case ( Map.lookup (from, dir) (specPaths spec') 
            , Map.lookup (to, reverseDirection dir) (specPaths spec') 
            ) of
      (Nothing, _) -> False
      (_, Nothing) -> False
      (Just p1, Just p2) -> pathTo p1 == Just to && 
                            pathKey p1 == Nothing && 
                            pathTo p2 == Just from && 
                            pathKey p2 == Nothing

-- | Check that a path adds correct locked connections
prop_correctLockedPath :: Property
prop_correctLockedPath = forAll worldSpecFilledGen $ \spec ->
  forAll arbitrary $ \dir ->
    let roomIds = getRoomIds (specRooms spec)
        (from, to) = case roomIds of
                      (x:y:_) -> (x,y)
                      _ -> error "worldSpecFilledGen guarantees at least 2 rooms"
        itm = case specItems spec of
                (x:_) -> x
                _ -> error "worldSpecFilledGen guarantees at least 1 item"
        spec' = execState (lockedPath from dir to itm) spec
    in case ( Map.lookup (from, dir) (specPaths spec') 
            , Map.lookup (to, reverseDirection dir) (specPaths spec') 
            ) of
      (Nothing, _) -> False
      (_, Nothing) -> False
      (Just p1, Just p2) -> pathTo p1 == Just to && 
                            pathKey p1 == Just itm && 
                            pathTo p2 == Just from && 
                            pathKey p2 == Just itm

-- | Check that correct open path and blocked path are added
prop_correctSlide :: Property
prop_correctSlide = forAll worldSpecFilledGen $ \spec ->
  forAll arbitrary $ \dir ->
    let roomIds = getRoomIds (specRooms spec)
        (from, to) = case roomIds of
                      (x:y:_) -> (x,y)
                      _ -> error "worldSpecFilledGen guarantees at least 2 rooms"
        spec' = execState (slide from dir to) spec
    in case ( Map.lookup (from, dir) (specPaths spec') 
            , Map.lookup (to, reverseDirection dir) (specPaths spec') 
            ) of
      (Nothing, _) -> False
      (_, Nothing) -> False
      (Just p1, Just p2) -> pathTo p1 == Just to && 
                            pathKey p1 == Nothing && 
                            pathTo p2 == Nothing

-- | Check that correct locked path and blocked path are added
prop_correctLockedSlide :: Property
prop_correctLockedSlide = forAll worldSpecFilledGen $ \spec ->
  forAll arbitrary $ \dir ->
    let roomIds = getRoomIds (specRooms spec)
        (from, to) = case roomIds of
                      (x:y:_) -> (x,y)
                      _ -> error "worldSpecFilledGen guarantees at least 2 rooms"
        itm = case specItems spec of
                (x:_) -> x
                _ -> error "worldSpecFilledGen guarantees at least 1 item"
        spec' = execState (lockedSlide from dir to itm) spec
    in case ( Map.lookup (from, dir) (specPaths spec') 
            , Map.lookup (to, reverseDirection dir) (specPaths spec') 
            ) of
      (Nothing, _) -> False
      (_, Nothing) -> False
      (Just p1, Just p2) -> pathTo p1 == Just to && 
                            pathKey p1 == Just itm && 
                            pathTo p2 == Nothing 
