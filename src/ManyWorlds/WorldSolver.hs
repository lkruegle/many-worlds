
module ManyWorlds.WorldSolver
  ( solveWorld,
  )
where

import Data.Maybe
import qualified Data.Set as S
import ManyWorlds.Internal
import ManyWorlds.InternalTypes

-- | Solves the given world providing a description of the result
--
-- Attempts to find a solution and returns that solution if one is found.
--
-- If the world is not solvable or partially solvable, all stuck states found
-- are returned. This means that some stuck states may be redundant as every
-- probe of the solver that does not find an end state generates a stuck state.
solveWorld :: World -> SolveResult
solveWorld w = case breadthFirstSolve (S.singleton w) [(w, [])] of
  -- Empty worlds are unsolvable, this shouldn't really be possible though
  ([], []) -> Unsolvable []
  ([], stucks) -> Unsolvable stucks
  ((_, path) : _, []) -> Solvable path
  ((_, path) : _, stucks) -> Partial path stucks

breadthFirstSolve ::
  -- | The World states that have been seen before
  S.Set World ->
  -- | The queue of worlds + actions taken i.e. path taken
  [(World, [Action])] ->
  -- | Accumulator of solutions and stuck states
  ([(World, [Action])], [StuckState])
breadthFirstSolve _ [] = ([], []) -- Base case
breadthFirstSolve visited ((world, path) : queue) =
  -- Pickup all Items in the current room
  let pickupWorld =
        foldl (\w a -> fromMaybe w (takeAction w a)) world (legalPickups world)
      -- List of all possible moves from current world
      actedWorlds =
        mapMaybe (\a -> fmap (a,) (takeAction pickupWorld a)) (legalMoves pickupWorld)
      -- Create the next worlds to add to the queue based on worlds'. This
      -- filters out worlds that are equivalent to previously visted worlds to
      -- avoid backtracking.
      searchNext =
        [ (world', action : path)
          | (action, world') <- actedWorlds,
            world' `S.notMember` visited
        ]
   in case checkEndConditions pickupWorld of
        -- Reverse the path if returned as it is accumulated backwards
        Just _ -> ([(world, reverse path)], [])
        -- Continue searching through the world-space
        Nothing -> case searchNext of
          [] ->
            -- current world cannot progress, add a StuckState to the
            -- result of continuing BFS.
            let (solves, stucks) = breadthFirstSolve visited queue
             in case stuckType world of
                  Just stuck -> (solves, stuck : stucks)
                  Nothing -> (solves, stucks)
          _ ->
            -- Add searchNext to the visited set, push to back of the queue
            -- And keep searching.
            let visited' = foldr (S.insert . fst) visited searchNext
             in breadthFirstSolve visited' (queue ++ searchNext)

-- | Gets all possible valid actions that can be taken in the current world.
legalMoves :: World -> [Action]
legalMoves world@(World _ state) =
  [Move (currentRoom state) d | (d, p) <- currentPaths world, canMove p]
  where
    canMove p = notLocked p && notBlocked p
    notBlocked p = isJust $ pathTo p
    notLocked p = case pathKey p of
      Just k -> k `elem` inventory world
      _ -> True

legalPickups :: World -> [Action]
legalPickups world = [PickUp itm | itm <- currentRoomItems world]

-- | Check if the world is actually stuck or just a previously visited state.
stuckType :: World -> Maybe StuckState
stuckType w@(World _ state) = case actualPaths of
  [] -> Just $ NoPath room
  paths -> case mapMaybe pathKey paths of
    [] -> Nothing
    itms -> Just $ NeedItems room itms
  where
    actualPaths = [p | (_, p) <- currentPaths w, isJust $ pathTo p]
    room = currentRoom state
