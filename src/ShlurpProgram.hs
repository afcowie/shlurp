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

main :: IO ()
main = do
    [owner,repo] <- getArgs
    config <- loadSettings (T.pack owner) (T.pack repo)

    issues <- executeGitHub config listIssues

    mapM_ (T.putStrLn . display) issues

display :: Issue -> Text
display issue =
  let
    title = issueTitle issue
    labels = T.intercalate ", " (V.toList (fmap labelName (issueLabels issue)))
    description = fromMaybe "~" (issueBody issue)
  in
    T.concat [title, "\n", labels, "\n\n", description]


