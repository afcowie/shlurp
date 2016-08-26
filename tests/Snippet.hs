{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as S
import Data.Maybe (isJust)
import qualified Data.Vector as V
import GitHub
import System.Environment

import Shlurp.Config
import Shlurp.Operations

main :: IO ()
main = do
    config <- loadSettings "afcowie" "tablinator"

    issues <- executeGitHub config listIssues


    mapM_ (print . issueTitle) issues
    mapM_ (print . V.map labelName . issueLabels) issues
