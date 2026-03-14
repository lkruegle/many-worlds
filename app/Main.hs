module Main (main) where

import ManyWorlds
import Data.Text.IO as TIO

main :: IO World
main =
  let (w, result) = buildWorld world
  in case result of
    Solvable _ -> do
      runWorld w
    Partial _ stucks -> do
      TIO.putStrLn "Game has dead ends. Stuck states:"
      print stucks
      TIO.putStrLn "Playing game with dead ends:\n"
      runWorld w
    Unsolvable stucks -> do
      TIO.putStrLn "Game is unsolvable. Stuck states:"
      print stucks
      return w

world :: WorldBuilder RoomId
world = do
  key <- item "rusty key"
  cloth <- item "dirty cloth"
  dungeon <- room "dungeon" "A dark, dank dungeon." [cloth, key]
  outside <- emptyRoom "outside" "The bright outdoors!"
  worseDungeon <- emptyRoom "worseDungeon" "A darker, danker dungeon."
  lockedPath dungeon North outside key
  lockedSlide dungeon West worseDungeon cloth
  endRoom outside "You escaped from the dark, dank dungeon!"
  return dungeon
