{-# LANGUAGE OverloadedStrings #-}

module Shlurp.Sorting
(
    groupByMilestone
)
where

import Data.List (sortBy, foldl')
import Data.Maybe (isJust, fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GitHub.Endpoints.Issues (Issue, issueLabels, issueTitle, issueBody, issueMilestone)
import GitHub.Endpoints.Issues (Milestone, milestoneTitle, milestoneDescription, milestoneDueOn, milestoneNumber)

groupByMilestone :: Vector Issue -> [(Milestone,[Issue])]
groupByMilestone = extractMilestonesInOrder . concentrateIssues

extractMilestonesInOrder :: Map Milestone [Issue] -> [(Milestone,[Issue])]
extractMilestonesInOrder milestones =
  let
    unordered = Map.keys milestones
    corrected = sortBy orderMilestones unordered

    extract :: [(Milestone,[Issue])] -> Milestone -> [(Milestone,[Issue])]
    extract acc key = (key, fromJust (Map.lookup key milestones)) : acc
  in
    foldl' extract [] corrected


--
-- Group issues under their respective Milestones. The catch is that we don't
-- control the Ord instance for Milestone, and we have a customer Ordering.
-- Hence the custom extract function above.
--
concentrateIssues :: Vector Issue -> Map Milestone [Issue]
concentrateIssues =
    V.foldl f Map.empty . V.filter (isJust . issueMilestone)
  where
    f :: Map Milestone [Issue] -> Issue -> Map Milestone [Issue]
    f acc issue =
      let
        milestone = (fromJust . issueMilestone) issue
      in
        case Map.lookup milestone acc of
            Just issues -> Map.insert milestone (issue:issues) acc
            Nothing     -> Map.insert milestone (issue:[]) acc

--
-- | For use with 'sortBy' function.
--
orderMilestones :: Milestone -> Milestone -> Ordering
orderMilestones =
    comparing (Down . milestoneDueOn)
    <> comparing milestoneDescription
    <> comparing milestoneNumber

