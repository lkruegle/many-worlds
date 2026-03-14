-- | WorldRunner contains the runFunctions for World built using ManyWorlds
--
-- While this module provides a default textual interface, you can customize
-- this using a WorldConfig. See ManyWorlds.WorldConfig for more details.
module ManyWorlds.WorldRunner
  ( runWorld,
    runWorld',
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ManyWorlds.Internal
import ManyWorlds.InternalTypes
import ManyWorlds.WorldConfig
import System.IO (hFlush, stdout)

-- | Default world running function, given a world, runs an IO loop to play
-- through the world.
--
-- This function relies on the default provided WorldConfig. If you want to
-- customize your own config functions, use `runWorld'` instead.
--
-- Returns a monadic value containing the world's end state.
runWorld :: World -> IO World
runWorld = runWorld' defaultConfig

-- | World run function that allows you to provide a custom WorlConfig
--
-- See `runWorld` for more details.
runWorld' :: WorldConfig -> World -> IO World
runWorld' config = worldLoop Look
  where
    getFeedbackText = provideFeedback config
    worldLoop action world = do
      storyText <- runReaderT (describeAction config action) world
      case checkEndConditions world of
        Just endText -> do
          printStory $ T.unlines [storyText, endText]
          return world
        Nothing -> do
          printStory storyText
          action' <- runReaderT (getNextAction getFeedbackText) world
          case takeAction world action' of
            Just world' -> worldLoop action' world'
            Nothing -> return world

-- | Asks the player what they want to do next.
-- Only allows actions that are available in the current room.
getNextAction :: (ActionFeedback -> WorldM Text) -> WorldM Action
getNextAction provideFeedback' = do
  liftIO $ TIO.putStrLn ""
  -- hFlush forces the printing of the partial line
  liftIO $ TIO.putStr "> " >> hFlush stdout
  input <- liftIO TIO.getLine
  result <- runExceptT $ parseAction input
  case result of
    Right action -> return action
    Left feedback -> do
      feedbackText <- provideFeedback' feedback
      lift $ printStory feedbackText
      getNextAction provideFeedback'

-- TODO: These parsers could theoretically be added to the config so that
-- custom interactions could be further fleshed out.

-- | Parse an action, providing feedback or the resulting action as a result.
parseAction :: Text -> FeedbackM Action
parseAction text = case T.words $ T.toLower text of
  ("go" : dtxt) -> buildMove $ T.unwords dtxt
  ("take" : itemname) -> buildPickup $ T.unwords itemname
  ("stuff" : _) -> return Inventory
  ("look" : _) -> return Look
  ("quit" : _) -> return Quit
  ("help" : _) -> return Help
  _ -> throwError $ InvalidAction text

-- | Further parse and build up a Move action, this function handles the
-- secondary arguments to the Move command.
buildMove :: Text -> FeedbackM Action
buildMove txt = do
  d <- case T.words $ T.toLower txt of
    ("north" : _) -> return North
    ("south" : _) -> return South
    ("east" : _) -> return East
    ("west" : _) -> return West
    _ -> throwError $ InvalidDirection txt
  (World spec state) <- lift ask
  let room = currentRoom state
  path <- case M.lookup (room, d) (specPaths spec) of
    Just path -> return path
    Nothing -> throwError $ NoSuchPath d
  case pathKey path of
    Just key ->
      if key `elem` heldItems state
        then return ()
        else throwError $ PathLocked d key
    Nothing -> return ()
  return $ Move room d

-- | Further parse and build up a PickUp action. This handles the secondary args
-- to the BuilUp.
buildPickup :: Text -> FeedbackM Action
buildPickup txt = do
  (World spec state) <- lift ask
  let roomd = M.lookup (currentRoom state) (specRooms spec)
  let items = maybe [] roomItems roomd
  case txt of
    "" -> throwError $ NoSuchItem txt
    _ -> case findItem txt items of
      Just itm -> return $ PickUp itm
      Nothing -> throwError $ NoSuchItem txt
  where
    findItem t =
      L.find (\(ItemId name) -> T.toLower t `T.isInfixOf` T.toLower name)

-- | Helper for keeping printing consistent
printStory :: Text -> IO ()
printStory t = liftIO $ TIO.putStrLn t
