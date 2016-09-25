{-# LANGUAGE OverloadedStrings #-}

module Shlurp.Summary
(
    outputMilestone,
    outputIssues,
    wrapParagraph
)
where

import Prelude hiding ((<$>))
import Data.List (foldl', intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Text.Lazy.Builder
import qualified Data.Vector as V
import GitHub.Endpoints.Issues (Issue, issueLabels, issueTitle, issueBody)
import GitHub.Endpoints.Issues (Milestone, milestoneTitle, milestoneDescription)
import GitHub.Endpoints.Issues.Labels (labelName)
import System.IO (stdout)

import Shlurp.Operations (Label)

__WIDTH__ :: Int
__WIDTH__ = 78

outputMilestone :: Milestone -> IO ()
outputMilestone = L.putStr . toLazyText . renderMilestone


outputIssues :: [Issue] -> IO ()
outputIssues = L.putStr . toLazyText . concatVertical . fmap renderIssue


renderMilestone :: Milestone -> Builder
renderMilestone milestone =
  let
    title = renderTitle '=' (milestoneTitle milestone)
    description = renderBody (milestoneDescription milestone)
  in
    concatVertical
        [ title
        , ""
        , description
        , ""
        ]


renderIssue :: Issue -> Builder
renderIssue issue =
  let
    title = renderTitle '-' (issueTitle issue)
    labels = renderLabels issue
    description = renderBody (issueBody issue)
  in
    concatVertical
        [ title
        , ""
        , labels
        , ""
        , description
        , ""
        ]

renderLabel :: Label -> Builder
renderLabel = fromText . labelName

renderLabels :: Issue -> Builder
renderLabels = enclose "_(" ")_" ","
    . fmap renderLabel
    . V.toList . issueLabels

enclose :: Builder -> Builder -> Builder -> [Builder] -> Builder
enclose before after between list =
  let
    middles = intersperse between list
  in
    before <> foldr (<>) after middles

renderBody :: Maybe Text -> Builder
renderBody = concatVertical
    . fmap wrapParagraph
    . T.lines
    . fromMaybe "~"

concatVertical :: [Builder] -> Builder
concatVertical = foldr (<>) "" . intersperse "\n"


wrapParagraph :: Text -> Builder
wrapParagraph = wrapHelper . T.words

wrapHelper :: [Text] -> Builder
wrapHelper [] = ""
wrapHelper [x]  = fromText x
wrapHelper (x:xs) = snd $ foldl' wrapLine (T.length x, fromText x) xs

wrapLine :: (Int, Builder) -> Text -> (Int, Builder)
wrapLine (pos,builder) word =
  let
    width = T.length word
    width' = pos + width + 1
  in
    if width' > __WIDTH__
        then (width , builder <> "\n" <> fromText word)
        else (width', builder <> " "  <> fromText word)

renderTitle :: Char -> Text -> Builder
renderTitle level title =
  let
    underline = fromText (T.map (\_ -> level) title)
    heading = fromText title
  in
    heading <> "\n" <> underline

