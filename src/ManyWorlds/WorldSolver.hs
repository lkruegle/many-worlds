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
         in case stuckType world of
                        Just stuck -> (solves, stuck : stucks)
                        Nothing -> (solves, stucks)
      _ ->
        -- Add nextWorlds to the visited set, push to back of the queue
        -- And keep searching.
        let visited' = foldr (S.insert . fst) visited nextWorlds
         in breadthFirstSolve visited' (queue ++ nextWorlds)
  where
    pickups = legalPickups world
    moves = legalMoves world
    pickupWorld = foldl (\w a -> fromMaybe w (takeAction w a)) world pickups
    -- Create a list of alternate worlds that represent taking each of the
    -- currently legal moves.
    worlds' =
        mapMaybe (\a -> fmap (a,) (takeAction pickupWorld a)) (legalMoves pickupWorld)
    -- Create the next worlds to add to the queue based on worlds'. This
    -- filters out worlds that are equivalent to previously visted worlds to
    -- avoid backtracking.
    nextWorlds =
      [ (world', action : path)
        | (action, world') <- worlds',
          world' `S.notMember` visited
      ]

-- | Gets all possible valid actions that can be taken in the current world.
legalMoves :: World -> [Action]
legalMoves world@(World _ state) =
    [Move (currentRoom state) d | (d, p) <- currentPaths world, canMove p]
  where
    canMove p = case pathKey p of
      Just k -> k `elem` inventory world
      _ -> True


legalPickups :: World -> [Action]
legalPickups world = [PickUp itm | itm <- currentRoomItems world]

-- | If a genuine stuck state, it
stuckType :: World -> Maybe StuckState
stuckType w@(World _ state) = case currentPaths w of
  [] -> Just $ NoPath room
  paths -> case mapMaybe pathKey [p | (_, p) <- paths] of
    [] -> Nothing
    itms -> Just $ NeedItems room itms
  where
    room = currentRoom state
