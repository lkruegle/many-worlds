{-# LANGUAGE TupleSections #-}

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
  ([], []) -> Unsolvable [] -- Empty worlds are unsolvable
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
  case checkEndConditions world of
    -- Reverse the path if returned as it is accumulated backwards
    Just _ -> ([(world, reverse path)], [])
    -- Continue searching through the world-space
    Nothing -> case nextWorlds of
      [] ->
        -- current world cannot progress, add a StuckState to the
        -- result of continuing BFS.
        -- TODO: Consider that the list of StuckStates will currently have an
        -- entry for *every* path that doesn't lead to a solution without
        -- backtracking. However, there are some paths that might exist in
        -- visited already but could eventually reach a solution. i.e. this
        -- will return "false positive" StuckStates.
        let (solves, stucks) = breadthFirstSolve visited queue
         in (solves, stuckType world : stucks)
      _ ->
        -- Add nextWorlds to the visited set, push to back of the queue
        -- And keep searching.
        let visited' = foldr (S.insert . fst) visited nextWorlds
         in breadthFirstSolve visited' (queue ++ nextWorlds)
  where
    -- Create a list of alternate worlds that represent taking each of the
    -- currently legal moves.
    worlds' = mapMaybe (\a -> fmap (a,) (takeAction world a)) (allActions world)
    -- Create the next worlds to add to the queue based on worlds'. This
    -- filters out worlds that are equivalent to previously visted worlds to
    -- avoid backtracking.
    nextWorlds =
      [ (world', (action : path))
        | (action, world') <- worlds',
          world' `S.notMember` visited
      ]

-- | Gets all possible valid actions that can be taken in the current world.
allActions :: World -> [Action]
allActions world@(World _ state) = legalPickups ++ legalMoves
  where
    canMove p = case pathKey p of
      Just k -> k `elem` inventory world
      _ -> True
    legalMoves =
      [Move (currentRoom state) d | (d, p) <- currentPaths world, canMove p]
    legalPickups = [PickUp itm | itm <- currentRoomItems world]

-- | Assuming the current World state is stuck, determine the cause.
stuckType :: World -> StuckState
stuckType w@(World _ state) = case currentPaths w of
  [] -> NoPath room
  paths -> NeedItems room $ mapMaybe pathKey [p | (_, p) <- paths]
  where
    room = currentRoom state
