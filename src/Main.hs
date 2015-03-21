{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
import System.IO
import Github.PullRequests hiding (Diff)
import qualified Github.Data.Definitions as GDD
import Github.Auth
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

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
            Right commits -> putStrLn (show pr) (show commits)

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
