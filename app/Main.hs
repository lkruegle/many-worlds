module Main (main) where

import ManyWorlds
import qualified Data.Text as T
import Data.Text.IO as TIO

main :: IO World
main = do
  let (w, result) = buildWorld world
  case result of
    Solvable _ -> return ()
    Partial _ stucks -> do
      TIO.putStrLn "Game has dead ends. Stuck states:"
      print stucks
      TIO.putStrLn "Playing game with dead ends:\n"
    Unsolvable stucks -> do
      TIO.putStrLn "Game is unsolvable. Stuck states:"
      print stucks
  runWorld w


world :: WorldBuilder RoomId
world = do
  key <- item "rusty key"
  cloth <- item "dirty cloth"
  rope <- item "grimy rope"
  potion <- item "vial of mysterious green liquid"
  dungeon <-
    room
      "dungeon"
      (T.unwords [
        "A dark, dank dungeon. There is a locked door through which you see a",
        "sliver of light. There's also a crack in the floor that you might be",
        "able to drop through. If only you had something to lower yourself",
        "down with..."
      ])
      [rope]
  outside <-
    emptyRoom
      "outside" "The sun glares down on you at the edge of a cliff face."
  worseDungeon <-
    room
      "worseDungeon"
      "An abandoned workshop. You see the remnants of experiments strewn about."
      [key, cloth, potion]
  stair <-
    emptyRoom
      "stair"
      "A spiral staircase that looks like it leads back to the dungeon..."
  dungeon' <-
    emptyRoom
      "dungeon'"
      (T.unwords [
        "A dark, dank dungeon. There is a locked door through which you see a",
        "sliver of light. There's also a crack in the floor that you might be",
        "able to drop through. Strange that you didn't notice the stairs",
        "before."
      ])
  lockedPath dungeon North outside key
  lockedSlide dungeon West worseDungeon rope
  path worseDungeon South stair
  -- dungeon' uses the fact that each path is really 2 paths to allow
  -- discovery of new rooms to replace a room you previously visited
  path stair West dungeon'
  lockedPath dungeon' North outside key
  lockedSlide dungeon' West worseDungeon rope
  endRoom outside "You escaped from the dungeon!"
  return dungeon
