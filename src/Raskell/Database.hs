{-# LANGUAGE OverloadedStrings #-}
module Raskell.Database where

import Control.Applicative
import Control.Monad (when, void)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Raskell.Types (Rating, User, UserProjectOverview(..), ratingUser, ratingProject)
import Snap.Snaplet.PostgresqlSimple

instance ToRow Rating where
  toRow r = [ toField $ ratingProject r
            , toField $ ratingUser r
            ]

instance FromRow UserProjectOverview where
  fromRow = UserProjectOverview <$> field <*> field <*> field

toggleRating :: (Functor m, HasPostgres m) => Rating -> m UserProjectOverview
toggleRating rating = do
  inserted <- execute "INSERT INTO rating (project, rater) SELECT project, rater FROM (VALUES (?, ?)) s(project, rater) WHERE (rater) NOT IN (SELECT rater FROM rating WHERE project = s.project)" rating
  when (inserted == 0) $ void $
    execute "DELETE FROM rating WHERE project = ? AND rater = ?" rating
  overview <- listToMaybe <$> query "SELECT project, count(rater) AS ratings, (? = any(array_agg(rater))) AS rated FROM rating WHERE project = ? GROUP BY project" (ratingUser rating, ratingProject rating)
  return $ case overview of
    Just o -> o
    Nothing -> UserProjectOverview { upoProject = ratingProject rating, upoRatings = 0, upoHaveRated = False }

userOverview :: (Functor m, HasPostgres m) => User -> m [UserProjectOverview]
userOverview rater =
  query "SELECT project, count(rater) AS ratings, ((SELECT login FROM raskell_key WHERE key = ?) = any(array_agg(rater))) AS rated FROM rating GROUP BY project" (Only rater)

addKey :: (Functor m, HasPostgres m) => Text -> String -> m ()
addKey login token = void $
  execute "INSERT INTO raskell_key (login, key) VALUES (?, ?)"
    (login, token)

resolveKeyToName :: (Functor m, HasPostgres m) => Text -> m (Maybe Text)
resolveKeyToName key =
  (listToMaybe . map fromOnly) <$>
    (query "SELECT login FROM raskell_key WHERE key = ?" $ Only key)

userKey :: (Functor m, HasPostgres m) => Text -> m String
userKey login = (fromOnly . head) <$>
  (query "SELECT key FROM raskell_key WHERE login = ?" (Only login))
