{-# LANGUAGE OverloadedStrings #-}

module Shlurp.Summary
(
    outputMilestone,
    outputIssues
)
where

import Prelude hiding ((<$>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub.Endpoints.Issues (Issue, issueLabels, issueTitle, issueBody)
import GitHub.Endpoints.Issues (Milestone, milestoneTitle, milestoneDescription)
import GitHub.Endpoints.Issues.Labels (labelName)
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen

import Shlurp.Operations (Label)

outputMilestone :: Milestone -> IO ()
outputMilestone = displayIO stdout
    . renderPretty 1.0 78
    . renderMilestone


outputIssues :: [Issue] -> IO ()
outputIssues = displayIO stdout
    . renderPretty 1.0 78
    . vsep . fmap renderIssue


renderMilestone :: Milestone -> Doc
renderMilestone milestone =
  let
    title = renderTitle '=' (milestoneTitle milestone)
    description = renderBody (milestoneDescription milestone)
  in
    vcat
        [ title
        , empty
        , description
        , linebreak
        ]


renderIssue :: Issue -> Doc
renderIssue issue =
  let
    title = renderTitle '-' (issueTitle issue)
    labels = renderLabels issue
    description = renderBody (issueBody issue)
  in
    vcat
        [ title
        , empty
        , labels
        , empty
        , description
        , linebreak
        ]

renderLabel :: Label -> Doc
renderLabel = text . T.unpack . labelName

renderLabels :: Issue -> Doc
renderLabels = enclose (string "_(") (string ")_")
    . fillSep . punctuate comma
    . fmap renderLabel
    . V.toList . issueLabels

renderBody :: Maybe Text -> Doc
renderBody = vcat
    . fmap wrapParagraphs
    . T.lines
    . fromMaybe "~"

wrapParagraphs :: Text -> Doc
wrapParagraphs = fillSep . fmap (text . T.unpack) . T.words

renderTitle :: Char -> Text -> Doc
renderTitle level title =
  let
    underline = T.unpack (T.map (\_ -> level) title)
    heading = T.unpack title
  in
    text heading <> linebreak <> text underline

