{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shlurp.Config 
(
    Config(..),
    loadSettings,
    GitHub,
    executeGitHub
)
where

import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Text (Text)
import qualified Data.Text as T
import GitHub.Data
import GitHub.Request
import System.Environment

data Config = Config {
    configToken :: Auth,
    configOwner :: Name Owner,
    configRepo  :: Name Repo
} deriving Show


loadSettings :: Text -> Text -> IO Config
loadSettings owner repo = do
    possibletoken <- lookupEnv "GITHUB_TOKEN"
    let token = case possibletoken of
                    Just token'  -> S.pack token'
                    Nothing -> error "Need to set GITHUB_TOKEN with a valid GitHub personal access token"
    return $ Config {
        configToken = OAuth token,
        configOwner = mkOwnerName owner,
        configRepo = mkRepoName repo
    }



newtype GitHub a = Wrap (StateT Config IO a) 
  deriving (Functor, Applicative, Monad, MonadState Config, MonadIO)

--
-- | Carry out GitHub commands
--
executeGitHub :: Config -> GitHub a -> IO a
executeGitHub config (Wrap monad) = do
    evalStateT monad config

