module Main where

import Control.Monad (forever)
import Control.Monad.State
import qualified Data.Text as T
import Lighthouse.Authentication
import Lighthouse.Connection
import Lighthouse.Display
import Lighthouse.Event
import Lighthouse.Utils.Color

-- | Handles a key event.
keyEvent :: KeyEvent -> LighthouseIO Display ()
keyEvent e = do
    display <- liftIO $ randomIO
    sendDisplay display

-- | Runs the main event-based game loop.
app :: LighthouseIO Display ()
app = forever $ do
    events <- receiveKeyEvents
    mapM keyEvent events

main :: IO ()
main = do
    args <- getArgs
    case args of
        [username, token] -> runLighthouseIO (flip runStateT emptyDisplay) $ Authentication { username = username, token = T.pack token }
        _ -> putStrLn "Arguments: [api username] [api token]"
