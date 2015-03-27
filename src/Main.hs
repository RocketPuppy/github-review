{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
import System.IO
import Github.PullRequests hiding (Diff)
import qualified Github.Data.Definitions as GDD
import Github.Auth
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Data.Functor
import Control.Monad
import Control.Monad.Catch
import Data.Algorithm.DiffOutput
import Data.FileStore

data CommandArgs = CommandArgs { usernameArg :: String
                               , passwordFlag :: Bool
                               , reviewArg :: Int
                               , ghOrganization :: String
                               , ghRepository :: String
                               }
    deriving (Show, Data, Typeable)

commandArgs = CommandArgs { usernameArg = def &= help "Github username" &= name "username" &= typ "STRING"
                          , passwordFlag = def &= help "Prompt for password" &= name "password"
                          , reviewArg = def &= typ "PULL_REQUEST_ID" &= argPos 0
                          , ghOrganization = def &= help "Github organization" &= name "organization" &= typ "STRING"
                          , ghRepository = def &= help "Github repository" &= name "repository" &= typ "STRING"
                          }

main :: IO ()
main = do
    CommandArgs username usePassword toReview organization repository <- cmdArgs commandArgs
    password <- if usePassword then getPassword else return ""

    let ghAuth = Just (GithubBasicAuth (BS.pack username) (BS.pack password))

    pr <- pullRequest' ghAuth organization repository toReview
    prCommits <- pullRequestCommits' ghAuth organization repository toReview

    case pr of
        Left _ -> error "Could not find PR with that ID"
        Right pr -> case prCommits of
            Left _ -> error "Could not get commits for PR"
            Right commits -> doReview pr commits

fileStore :: FileStore
fileStore = gitFileStore "."

doReview :: DetailedPullRequest -> [GDD.Commit] -> IO ()
doReview pr commits = do
    -- Commits listing
    let title = "PR: #" ++ (show . detailedPullRequestNumber $ pr)
        head = pullRequestCommitSha . detailedPullRequestHead $ pr
        base = pullRequestCommitSha . detailedPullRequestBase $ pr

    title <- plainText . T.pack $ title
    commitsShas <- flip newTextList 1 . reverse . map (T.pack . commitSha) $ commits

    commitsPane <- return title <--> hBorder <--> return commitsShas

    commitView <- newTextList [] 1

    commits <- return commitsPane <++> vBorder <++> return commitView

    fg <- newFocusGroup
    addToFocusGroup fg commitsShas
    addToFocusGroup fg commitView

    c <- newCollection
    addToCollection c commits fg

    commitsShas `onSelectionChange` \(SelectionOn list text widget) -> do
        rev <- revision fileStore (T.unpack $ T.replace "\"" "" text)
        let tr = TimeRange Nothing . Just . revDateTime $ rev
        let files = map filePathFromChange . revChanges $ rev
        diffs <- mapM (diffPathAt fileStore rev) . revChanges $ rev
        clearList commitView
        diffLines <- concat . concat <$> mapM (\(path, diff) -> sequence $ mapM id [plainText T.empty, plainText (T.pack path) >>= withNormalAttribute (bgColor yellow)] : map colorDiff diff) (zip files diffs)
        mapM_ (addToList commitView "not used") diffLines

    fg `onKeyPressed` \_ key _ ->
        if key == KChar 'q'
            then shutdownUi >> return True
            else return False

    runUi c defaultContext

colorDiff :: Diff [String] -> IO [Widget FormattedText]
colorDiff (First changes) = mapM (withNormalAttribute (bgColor red) <=< plainText . T.pack) changes
colorDiff (Second changes) = mapM (withNormalAttribute (bgColor green) <=< plainText . T.pack) changes
colorDiff (Both changes _) = mapM (plainText . T.pack) changes

diffPathAt :: FileStore -> Revision -> Change -> IO [Diff [String]]
diffPathAt fs  (Revision {revId = revId}) (Added fp) = diff fs fp Nothing (Just revId)
diffPathAt fs parent change = do
    -- if something is deleted/modified it must have at least two commits, so head is safe
    base <- head . reverse <$> history fs [fp] tr (Just 2)
    diff fs fp (Just $ revId base) (Just $ revId parent)
    where
        fp = filePathFromChange change
        tr = TimeRange Nothing . Just . revDateTime $ parent

filePathFromChange :: Change -> FilePath
filePathFromChange (Added fp) = fp
filePathFromChange (Deleted fp) = fp
filePathFromChange (Modified fp) = fp

-- SO: http://stackoverflow.com/questions/4064378/prompting-for-a-password-in-haskell-command-line-application
getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
