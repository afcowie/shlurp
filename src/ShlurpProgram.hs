module Main where

import qualified Data.Text as T
import System.Environment

import Shlurp.Config

main :: IO ()
main = do
    [owner,repo] <- getArgs
    config <- loadSettings (T.pack owner) (T.pack repo)
    print owner
    print repo
    print config
