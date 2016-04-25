{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as S
import Data.Maybe (isJust)
import qualified Data.Vector as V
import GitHub
import System.Environment

import Shlurp.Config

main :: IO ()
main = do
--  let token1 = S.pack "0000000000000000000000000000000000000000"
--  token2 <- S.readFile "GITHUB_TOKEN" >>= return . head . S.lines
    possibletoken <- lookupEnv "GITHUB_TOKEN"
    let token = case possibletoken of
                    Just token'  -> S.pack token'
                    Nothing -> error "Need to set GITHUB_TOKEN with a valid GitHub personal access token"

--  possibleUser <- executeRequest (OAuth token) (userInfoCurrentR)
--  print possibleUser

    possibleIssues <- executeRequest (OAuth token) (issuesForRepoR "afcowie" "http-streams" [] Nothing)
    let issues = case possibleIssues of
                    Right issues  -> V.filter (not . isJust . issuePullRequest) issues
                    Left problem  -> error (show problem)

    mapM_ (print . issueTitle) issues
