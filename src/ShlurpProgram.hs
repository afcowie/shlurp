{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import GitHub.Data
import System.Environment

import Shlurp.Config
import Shlurp.Operations
import Shlurp.Summary

main :: IO ()
main = do
    [owner,repo] <- getArgs
    config <- loadSettings (T.pack owner) (T.pack repo)

    issues <- executeGitHub config listIssues

    outputIssues (V.toList issues)

