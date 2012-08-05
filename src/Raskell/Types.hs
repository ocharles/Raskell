module Raskell.Types where

import Data.Text (Text)

type Project = Text
type User = Text

data Rating = Rating
    { ratingUser :: User
    , ratingProject :: Project
    } deriving (Eq, Show)

data UserProjectOverview = UserProjectOverview
    { upoProject :: Project
    , upoRatings :: Integer
    , upoHaveRated :: Bool
    } deriving (Show)
