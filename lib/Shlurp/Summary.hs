module Shlurp.Summary
(
    display
)
where

import Prelude hiding ((<$>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GitHub.Endpoints.Issues (Issue, issueLabels, issueTitle, issueBody)
import GitHub.Endpoints.Issues.Labels (labelName)
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen

import Shlurp.Operations (Label)

display :: Vector Issue -> IO ()
display = displayIO stdout . renderPretty 1.0 78
    . vsep . fmap renderIssue . V.toList

renderIssue :: Issue -> Doc
renderIssue issue =
  let
    title = renderTitle issue
    labels = renderLabels issue
    description = renderBody issue
  in
    title <$> labels <$> description

renderLabel :: Label -> Doc
renderLabel = text . T.unpack . labelName

renderLabels :: Issue -> Doc
renderLabels = list . fmap renderLabel . V.toList . issueLabels

renderBody :: Issue -> Doc
renderBody = fillSep
    . fmap (text . T.unpack)
    . T.words
    . fromMaybe "~" . issueBody


renderTitle :: Issue -> Doc
renderTitle issue =
  let
    title = issueTitle issue
    underline = T.unpack (T.map (\c -> '-') title)
    heading = T.unpack title
  in
    text heading <> linebreak <> text underline <> linebreak
