{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader
import Data.List (sortOn)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Text.Printf (printf)

import qualified GitHub.Endpoints.Issues as GitHub
import GitHub.Data.Name (Name(N))

mcubootQuery :: Context
mcubootQuery = Context "d3zd3z" "runtimeco" "mcuboot"

zephyrQuery :: Context
zephyrQuery = Context "d3zd3z" "zephyrproject-rtos" "zephyr"

type QueryIO a = ReaderT Context IO a

main :: IO ()
main = do
   runReaderT askGitHub mcubootQuery
   runReaderT askGitHub zephyrQuery

askGitHub :: QueryIO ()
askGitHub = do
   user <- asks queryUser
   owner <- asks queryOwner
   project <- asks queryProject
   issues <- lift $ crashOr $
      -- GitHub.issuesForRepo "zephyrproject-rtos" "zephyr" $
      GitHub.issuesForRepo (N owner) (N project) $
         GitHub.IssueRepoMod $ \o -> o {
            GitHub.issueRepoOptionsAssignee = GitHub.FilterBy (N user),
            GitHub.issueRepoOptionsState = Nothing }
   lift $ T.putStrLn $ "Issues at [https://github.com/" <> owner <> "/" <> user <> "/issues\n"
   lift $ putStrLn $ header
   lift $ putStrLn $ foldMap ((<> "\n") . formatIssue) $ sortOn GitHub.issueNumber $ V.toList issues

data Context = Context {
   queryUser :: Text,
   queryOwner :: Text,
   queryProject :: Text }
   deriving Show

-- Turn an error into a user exception and throw it.
crashOr :: Show e => IO (Either e a) -> IO a
crashOr m = either (die . show) return =<< m

die :: String -> IO a
die = ioError . userError . ("GitHub error: " ++) . show

header :: String
header = "||" <> foldMap (<> "||") ["Issue", "Description", "Status", "Milestone"]

formatIssue :: GitHub.Issue -> String
formatIssue issue =
   let state = colorize $ GitHub.issueState issue in
   let number = GitHub.issueNumber issue in
   let url = unpack $ GitHub.getUrl $ GitHub.issueUrl issue in
   let title = jiraEscape $ unpack $ GitHub.issueTitle issue in
   let milestone = maybe " " id $ fmap GitHub.milestoneTitle $ GitHub.issueMilestone issue in
   printf "|[%d|%s]|%s|%s|%s|"
      number url title state milestone

colorize :: GitHub.IssueState -> String
colorize GitHub.StateOpen = "{color:red}OPEN{color}"
colorize GitHub.StateClosed = "{color:green}CLOSED{color}"

-- Escape some characters so they can be included in JIRA markup.
-- This isn't perfect, and in general it is difficult to do with their
-- markup language.
jiraEscape :: String -> String
jiraEscape (x:xs)
   | x `elem` ("-*_?[]{}" :: String) = '\\' : x : jiraEscape xs
   | otherwise           = x : jiraEscape xs
jiraEscape [] = []
