module Shlurp.Operations 
(
    listIssues,
    Label,
    listLabels
)
where

import Control.Monad.State
import Data.Maybe (isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GitHub.Endpoints.Issues
import GitHub.Endpoints.Issues.Labels
import GitHub.Request

import Shlurp.Config

type Label = IssueLabel

listIssues :: GitHub (Vector Issue)
listIssues = do
    config <- get
    let token = configToken config
    let request = issuesForRepoR (configOwner config) (configRepo config) [] Nothing

    possibleIssues <- liftIO $ executeRequest token request

    let issues = case possibleIssues of
                    Right issues  -> V.filter (not . isJust . issuePullRequest) issues
                    Left problem  -> error (show problem)
    return issues

listLabels :: GitHub (Vector Label)
listLabels = do
    config <- get
    let token = configToken config
    let request = labelsOnRepoR (configOwner config) (configRepo config) Nothing

    possibleLabels <- liftIO $ executeRequest token request

    let labels = case possibleLabels of
                    Right labels  -> labels
                    Left problem  -> error (show problem)
    return labels



