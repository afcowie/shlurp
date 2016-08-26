module Shlurp.Operations 
(
    listIssues
)
where

import Control.Monad.State
import Data.Maybe (isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GitHub.Endpoints.Issues
import GitHub.Request

import Shlurp.Config

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



