module Main where

import qualified Codec.Picture as P
import Control.Monad (void)
import Control.Monad.Except (liftEither)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except
import qualified Data.Text as T
import Lighthouse.Authentication
import Lighthouse.Connection
import Lighthouse.Display
import Lighthouse.Utils.Color
import Lighthouse.Utils.General (liftMaybe)
import System.Environment (getArgs, getEnv)
import System.Random

-- | Renders a single image to the lighthouse.
app :: String -> LighthouseIO ()
app imagePath = do
    res <- runExceptT $ do
        dimg <- liftEither =<< liftIO (P.readPng imagePath)
        d <- liftEither $ dynImgToDisplay dimg
        lift $ sendDisplay d
        
    case res of
        Left e -> liftIO $ putStrLn e
        _ -> return ()
    
    -- DEBUG
    requestInputStream
    -- sendClose

dynImgToDisplay :: P.DynamicImage -> Either String Display
dynImgToDisplay dimg = case dimg of
    P.ImageRGB8  img -> Right $ imgToDisplay img $ \(P.PixelRGB8 r g b)    -> mkColor r g b
    P.ImageRGBA8 img -> Right $ imgToDisplay img $ \(P.PixelRGBA8 r g b _) -> mkColor r g b
    _                -> Left "Unrecognized PNG format"
    where mkColor r g b = Color (fromIntegral r) (fromIntegral g) (fromIntegral b)

imgToDisplay :: P.Pixel a => P.Image a -> (a -> Color) -> Display
imgToDisplay img pixToColor = Display $ rowAt <$> [0..lighthouseRows - 1]
    where width     = P.imageWidth img
          height    = P.imageHeight img
          rowAt y   = Row $ (pixAt y) <$> [0..lighthouseCols - 1]
          pixAt y x = pixToColor $ P.pixelAt img
            ((x * width) `div` lighthouseCols)
            ((y * height) `div` lighthouseRows)

main :: IO ()
main = do
    -- Fetch credentials from env vars
    username <- T.pack <$> getEnv "LIGHTHOUSE_USERNAME"
    token    <- T.pack <$> getEnv "LIGHTHOUSE_TOKEN"
    let auth = Authentication { username = username, token = token }

    -- Render image to lighthouse
    args <- getArgs
    case args of
        [imagePath] -> runLighthouseIO [listener] auth
            where listener = emptyListener { onConnect = app imagePath
                                           , onError   = \e -> liftIO $ putStrLn $ "Error from server: " ++ T.unpack e
                                           , onWarning = \w -> liftIO $ putStrLn $ "Warning from server: " ++ T.unpack w
                                           }
        _           -> putStrLn "Arguments: [path to png image]"
