{-# LANGUAGE RecordWildCards #-}
module Main where

import Lighthouse.Authentication
import Lighthouse.Connection
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [username, token] -> runLighthouseIO app $ Authentication {..}
        _ -> putStrLn "Arguments: [api username] [api token]"

app :: LighthouseIO ()
app = return ()
