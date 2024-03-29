{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Codec.Picture as P
import Control.Monad (void)
import Control.Monad.Except (liftEither)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except
import qualified Data.Text as T
import Lighthouse.Connection
import Lighthouse.Display
import Lighthouse.Options
import Lighthouse.Utils.Color
import Lighthouse.Utils.General (liftMaybe)
import Lighthouse.Utils.Logging (simpleLogHandler, infoLevel, logInfo, logError, runExceptTOrLog)
import System.Environment (getArgs, getEnv)
import System.Random

-- | Renders a single image to the lighthouse.
app :: String -> Listener ()
app imagePath = mempty
    { onConnect = do
        -- Render image to lighthouse
        runExceptTOrLog $ do
            dimg <- liftEither =<< liftIO (P.readPng imagePath)
            d <- liftEither $ dynImgToDisplay dimg
            lift $ sendDisplay d
        
        -- Required to get input messages
        requestStream
    , onInput = \e -> logInfo "app" $ "Got input event: " <> T.pack (show e)
    }
    -- sendClose

dynImgToDisplay :: P.DynamicImage -> Either String Display
dynImgToDisplay dimg = case dimg of
    P.ImageRGB8  img -> Right $ imgToDisplay img $ \(P.PixelRGB8 r g b)    -> mkColor r g b
    P.ImageRGBA8 img -> Right $ imgToDisplay img $ \(P.PixelRGBA8 r g b _) -> mkColor r g b
    _                -> Left "Unrecognized PNG format"
    where mkColor r g b = Color (fromIntegral r) (fromIntegral g) (fromIntegral b)

imgToDisplay :: P.Pixel a => P.Image a -> (a -> Color) -> Display
imgToDisplay img pixToColor = generateDisplay pixAt
    where width     = P.imageWidth img
          height    = P.imageHeight img
          pixAt x y = pixToColor $ P.pixelAt img
            ((x * width) `div` lighthouseCols)
            ((y * height) `div` lighthouseRows)

main :: IO ()
main = do
    -- Fetch credentials from env vars
    username <- T.pack <$> getEnv "LIGHTHOUSE_USER"
    token    <- T.pack <$> getEnv "LIGHTHOUSE_TOKEN"
    let auth = Authentication { authUsername = username, authToken = token }
        opts = (defaultOptions auth ()) { optLogHandler = simpleLogHandler infoLevel }

    -- Run the lighthouse app (that renders an image to the lighthouse)
    args <- getArgs
    case args of
        [imagePath] -> runLighthouseApp (app imagePath) opts
        _           -> putStrLn "Arguments: [path to png image]"
