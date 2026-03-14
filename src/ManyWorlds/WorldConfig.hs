-- | Provides the API for configuring the textual interface of your world.
--
-- This module provides a default config that implements some simple
-- action and feedback text to display to players. Users can define custom
-- configs to change the text that is not declared in the world builder.
module ManyWorlds.WorldConfig
  ( -- * Default Config
    defaultConfig,

    -- * Config Type
    WorldConfig (..),

    -- * Types for Custom Config
    Action,
    ActionFeedback,
    WorldM,
    FeedbackM,
    World,
    WorldSpec (),
    PlayerState (),
  )
where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import ManyWorlds.Internal
import ManyWorlds.Types
import Prelude hiding (unlines)

-- | Returns the default configuration for a world.
--
-- This provides default functions for interpreting game actions and player
-- feedback.
--
-- Users can override these functions to define custom interfaces.
defaultConfig :: WorldConfig
defaultConfig =
  WorldConfig
    { provideFeedback = provideFeedback',
      describeAction = describeAction'
    }

-- | Default feedback function
provideFeedback' :: ActionFeedback -> WorldM Text
provideFeedback' feedback = do
  return $ case feedback of
    InvalidDirection txt -> case txt of
      "" -> "Where do you want to go?"
      _ -> "\"" <> txt <> "\" is not a valid direction."
    NoSuchPath _ -> "There isn't a path that way."
    PathLocked _ _ -> "Can't open the door, maybe you need a key..."
    PathBlocked _ -> "The way is impassible."
    NoSuchItem txt -> case txt of
      "" -> "What do you want to take?"
      _ -> "You don't see a " <> txt <> " anywhere."
    InvalidAction _ -> "You don't know how to do that."

-- | Default action description function
describeAction' :: Action -> WorldM Text
describeAction' action = do
  world <- ask
  case action of
    Move r direction -> moveText r direction
    PickUp (ItemId itmname) -> return $ "You pick up the " <> itmname <> "."
    Look -> lookText
    Inventory -> return $ case inventory world of
      [] -> "Your inventory is empty!"
      itms -> "In your inventory, you have a " <> (itemListText itms) <> ""
    Quit -> return "You quit."
    Help -> return helpText

-- | Creates text describing moving from the room in that direction
moveText :: RoomId -> Direction -> WorldM Text
moveText room direction = do
  world@(World spec _) <- ask
  let path = case M.lookup (room, direction) (specPaths spec) of
        Just p -> p
        Nothing -> error $ "Invalid path: " <> show (room, direction)
  let key = pathKey path
  let text = case pathTo path of
        Just _ -> "You go " <> T.show direction <> "."
        Nothing -> "The path is blocked. You can't go that direction."
  let moveTexts =
        [text, currentRoomDesc world]
  return $ unlines $ case key of
    Just (ItemId name) ->
      ( ("The " <> name <> " unlocked the door!") : moveTexts
      )
    Nothing -> moveTexts

-- | Creates text describing looking around in the current room
lookText :: WorldM Text
lookText = do
  world <- ask
  let roomText = roomDesc (currentRoomData world)
  let itemText = case currentRoomItems world of
        [] -> "The room is empty."
        itms -> "You see a " <> itemListText itms <> "."
  let validDirections = [d | (d, _) <- currentPaths world]
  let pathText = directionListText validDirections
  return $ unlines [roomText, itemText, pathText]

-- | Converts a list of directions in a description of what directions the
-- player can move in.
directionListText :: [Direction] -> Text
directionListText ds = case ds of
  [] -> "There's no way out!"
  (d' : []) -> "There's a path to the " <> T.show d' <> "."
  (d' : ds') ->
    "There are paths to the "
      <> T.intercalate ", " [T.show d | d <- ds']
      <> ", and "
      <> T.show d'

-- | A generic list of items as text
itemListText :: [ItemId] -> Text
itemListText = \case
  (itm : []) -> itemText itm
  (itm : itms) ->
    ( T.intercalate ", a " (map itemText itms)
    )
      <> ", and a "
      <> itemText itm
  _ -> ""
  where
    itemText (ItemId a) = a

-- | Help text for explaining how to play the game to the player.
helpText :: Text
helpText =
  T.unlines
    [ "Type what you want to do.",
      "You can type 'go', 'take', 'stuff', 'look', 'help', or 'quit'."
    ]

-- | Use proprietary unlines to avoid \n after final item
unlines :: [Text] -> Text
unlines ts = T.intercalate "\n" ts
