-- This file is part of purebred
-- Copyright (C) 2018 Róman Joost
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleContexts #-}
module Purebred.System.Directory
  ( listDirectory'
  , filePathToEntry
  , maildirMessageFileTemplate
  ) where

import Control.Exception.Base (IOException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Exception (try)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import System.Process.Typed (readProcessStdout, proc)
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import Data.Char (isControl, isSpace)
import Data.List (sort, intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Error
import Types


listDirectory' :: (MonadError Error m, MonadIO m) => FilePath -> m [FileSystemEntry]
listDirectory' path = liftIO (try $ listDirectory path)
                      >>= either (throwError . convertError) (fmap sort <$> traverse (filePathToEntry path))
  where convertError :: IOException -> Error
        convertError = GenericError . show

filePathToEntry :: (MonadIO m) => FilePath -> FilePath -> m FileSystemEntry
filePathToEntry base filename = do
    exists <- liftIO $ doesDirectoryExist (base </> filename)
    pure $ if exists then Directory filename else File filename

-- | Generates a Maildir filename
-- see https://cr.yp.to/proto/maildir.html
maildirMessageFileTemplate :: MonadIO m => m FilePath
maildirMessageFileTemplate = do
  left <- liftIO $ formatTime defaultTimeLocale "%s" <$> getCurrentTime
  right <- getHostname
  pure $ intercalate "." [left, "", right]

getHostname :: (MonadIO m) => m String
getHostname = do
    (exitc, out) <- readProcessStdout (proc "hostname" [])
    case exitc of
      ExitSuccess -> pure (decode out)
      ExitFailure _ -> pure "localhost"
  where
    decode = C8.unpack . C8.filter (\x -> not (isControl x || isSpace x)) . LB.toStrict
