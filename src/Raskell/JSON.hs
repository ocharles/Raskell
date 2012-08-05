{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Raskell.JSON where

import Control.Applicative
import Data.Aeson
import Raskell.Types

instance FromJSON Rating where
  parseJSON j = case j of
    Object o -> Rating <$> o .: "user" <*> o .: "project"
    _ -> error "Invalid JSON"

instance ToJSON UserProjectOverview where
  toJSON upo = object [ "project" .= upoProject upo
                      , "ratings" .= upoRatings upo
                      , "rated" .= upoHaveRated upo
                      ]

instance ToJSON [UserProjectOverview] where
  toJSON upos = object $ map project upos
    where project upo = upoProject upo .= object [ "ratings" .= upoRatings upo
                                                 , "rated" .= upoHaveRated upo
                                                 ]
