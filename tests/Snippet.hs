{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as S
import GitHub
import System.Environment

import Shlurp.Config

main :: IO ()
main = do
--  let token1 = S.pack "0000000000000000000000000000000000000000"
--  token2 <- S.readFile "GITHUB_TOKEN" >>= return . head . S.lines
    token' <- lookupEnv "GITHUB_TOKEN"
    let token = case token' of
                    Just t  -> S.pack t
                    Nothing -> error "Need to set GITHUB_TOKEN with a valid GitHub personal access token"

    possibleUser <- executeRequest (OAuth token) (userInfoForR "afcowie")
    print possibleUser
