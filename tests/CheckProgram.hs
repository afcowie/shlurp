{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import Shlurp.Nothing

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "Stuff" $ do
        it "can be done with things" $ do
            True `shouldBe` True
