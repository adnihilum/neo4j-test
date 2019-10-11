module Lib
  ( someFunc
  ) where

import Control.Monad.IO.Class
import qualified NeoTest

someFunc :: IO ()
someFunc = do
  putStrLn "Hello, World!"
  NeoTest.run
  return ()
