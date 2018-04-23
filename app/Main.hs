{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sortOn)
import Data.Monoid ((<>))
import Data.Text (unpack)
import qualified Data.Vector as V
import Text.Printf (printf)

import qualified GitHub.Endpoints.Issues as Github

main :: IO ()
main = do
   issues <- crashOr $
      -- Github.issuesForRepo "zephyrproject-rtos" "zephyr" $
      Github.issuesForRepo "runtimeco" "mcuboot" $
         Github.IssueRepoMod $ \o -> o {
            Github.issueRepoOptionsAssignee = Github.FilterBy "d3zd3z",
            Github.issueRepoOptionsState = Nothing }
   putStrLn $ "Issues at [https://github.com/runtimeco/mcuboot/issues]\n"
   putStrLn $ header
   putStrLn $ foldMap ((<> "\n") . formatIssue) $ sortOn Github.issueNumber $ V.toList issues

-- Turn an error into a user exception and throw it.
crashOr :: Show e => IO (Either e a) -> IO a
crashOr m = either (die . show) return =<< m

die :: String -> IO a
die = ioError . userError . ("Github error: " ++) . show

header :: String
header = "||" <> foldMap (<> "||") ["Issue", "Description", "Status", "Milestone"]

formatIssue :: Github.Issue -> String
formatIssue issue =
   let state = colorize $ Github.issueState issue in
   let number = Github.issueNumber issue in
   let url = unpack $ Github.getUrl $ Github.issueUrl issue in
   let title = jiraEscape $ unpack $ Github.issueTitle issue in
   let milestone = maybe " " id $ fmap Github.milestoneTitle $ Github.issueMilestone issue in
   printf "|[%d|%s]|%s|%s|%s|"
      number url title state milestone

colorize :: Github.IssueState -> String
colorize Github.StateOpen = "{color:red}OPEN{color}"
colorize Github.StateClosed = "{color:green}CLOSED{color}"

-- Escape some characters so they can be included in JIRA markup.
-- This isn't perfect, and in general it is difficult to do with their
-- markup language.
jiraEscape :: String -> String
jiraEscape (x:xs)
   | x `elem` ("-*_?[]{}" :: String) = '\\' : x : jiraEscape xs
   | otherwise           = x : jiraEscape xs
jiraEscape [] = []
