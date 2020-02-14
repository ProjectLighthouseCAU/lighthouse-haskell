{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import Lighthouse.Authentication
import Lighthouse.Connection
import Lighthouse.Display (coloredDisplay)
import Lighthouse.Utils.Color
import System.Environment (getArgs)
import System.Random

app :: LighthouseIO ()
app = do
    display <- liftIO $ randomIO
    sendDisplay display
    sendClose

main :: IO ()
main = do
    args <- getArgs
    case args of
        [username, token] -> runLighthouseIO app $ Authentication { username = T.pack username, token = T.pack token }
        _ -> putStrLn "Arguments: [api username] [api token]"
