{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub as GH
import Shlurp.Nothing ()

main :: IO ()
main = do
    possibleUser <- GH.executeRequest' $ GH.userInfoForR "afcowie"
    print possibleUser


