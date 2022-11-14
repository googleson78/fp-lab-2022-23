{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Functor (void)
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny qualified as Reactive

main :: IO ()
main = do
  startGUI
    defaultConfig
      { jsPort = Just 42069
      }
    \window -> do
      (e, eh) <- liftIO Reactive.newEvent
      be <- stepper "" e
      zeIn <- UI.input
      zeOut <- UI.span # sink UI.text be
      on UI.valueChange zeIn (liftIO . eh)
      void $ getBody window #+ [element zeIn, element zeOut]
