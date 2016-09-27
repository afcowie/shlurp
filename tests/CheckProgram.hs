{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Test.Hspec

import Shlurp.Config
import Shlurp.Summary

main :: IO ()
main = hspec suite

sample = "This is a test of the emergency broadcast system. \
    \Do not be alarmed. Just panic"

force :: Builder -> Text
force = L.toStrict . toLazyText

suite :: Spec
suite = do
    describe "Word wrapping" $ do
        it "handles single short word" $ do
            (force (wrapParagraph 20 "Emergency")) `shouldBe` "Emergency"

        it "handles single long word" $ do
            (force (wrapParagraph 4 "Emergency")) `shouldBe` "Emergency"

        it "doesn't wrap short input" $ do
            (force (wrapParagraph 20 "This is a test")) `shouldBe` "This is a test"

        it "works at zero margin" $ do
            (force (wrapParagraph 0 "This is a test")) `shouldBe` "This\nis\na\ntest"

        it "wraps a full text" $ do
            (force (wrapParagraph 18 sample)) `shouldBe` "\
\This is a test of\n\
\the emergency\n\
\broadcast system.\n\
\Do not be alarmed.\n\
\Just panic\
\"
