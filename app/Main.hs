module Main where

import Lib
import App
import DB

import           System.Environment           (getArgs)

-- stack exec puzzle-server-exe migrate

main :: IO ()
main = do
  args <- getArgs
  let arg1 = if (length args > 0) then Just (args !! 0) else Nothing
  case arg1 of
        Just "migrate" -> doMigration
        _ -> startApp
