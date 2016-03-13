{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
import System.IO
import GitHub.Endpoints.PullRequests hiding (Diff)
import qualified GitHub.Data.GitData as GDD
import qualified GitHub.Data.Definitions as GDD
import qualified GitHub.Data.Repos as GDD
import qualified GitHub.Data.PullRequests as GDD
import GitHub.Data.Name (mkName, untagName)
import GitHub.Data.Id (mkId)
import GitHub.Auth
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
import qualified Data.Vector as V
import Data.Proxy (Proxy(..))

mkPullRequestId = mkId (Proxy :: Proxy GDD.PullRequest)

data CommandArgs = CommandArgs { token :: String
                               , reviewArg :: Int
                               , ghOrganization :: String
                               , ghRepository :: String
                               }
    deriving (Show, Data, Typeable)

commandArgs = CommandArgs { token = def &= help "Github API access token" &= name "token" &= typ "STRING"
                          , reviewArg = def &= typ "PULL_REQUEST_ID" &= argPos 0
                          , ghOrganization = def &= help "Github organization" &= name "organization" &= typ "STRING"
                          , ghRepository = def &= help "Github repository" &= name "repository" &= typ "STRING"
                          }

main :: IO ()
main = do
    args <- cmdArgs commandArgs
    let organization = mkOwnerName . T.pack . ghOrganization $ args
    let repository = mkRepoName . T.pack . ghRepository $ args
    let toReview = mkPullRequestId . reviewArg $ args
    let ghAuth = Just . OAuth . BS.pack . token $ args

    pr <- pullRequest' ghAuth organization repository toReview
    prCommits <- pullRequestCommits' ghAuth organization repository toReview

    case pr of
        Left e -> error $ "Could not find PR with that ID" ++ show e
        Right pr -> case prCommits of
            Left _ -> error "Could not get commits for PR"
            Right commits -> doReview pr (V.toList commits)

fileStore :: FileStore
fileStore = gitFileStore "."

doReview :: PullRequest -> [GDD.Commit] -> IO ()
doReview pr commits = do
    -- Commits listing
    let title = "PR: #" ++ (show . pullRequestNumber $ pr)
        head = pullRequestCommitSha . pullRequestHead $ pr
        base = pullRequestCommitSha . pullRequestBase $ pr

    title <- plainText . T.pack $ title
    commitsShas <- flip newTextList 1 . reverse . map (untagName . commitSha) $ commits

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
