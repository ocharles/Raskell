{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Raskell.Forms where

import Control.Applicative
import Data.Text
import Data.Text.Encoding
import Raskell.Snaplet
import Snap.Snaplet
import Snap.Snaplet.Auth
import Text.Digestive
import Text.Blaze.Html

data Registration = Registration
    { regUserName :: Text
    , regPassword :: Text
    }

data Login = Login
    { loginName :: Text
    , loginPassword :: Text
    }

registrationForm :: Monad m => Form Html m Registration
registrationForm =
    Registration
      <$> "username" .: text Nothing
      <*> "password" .: passwordConfirmer
  where passwordConfirmer =
          validate fst' $ (,) <$> ("p1" .: text Nothing)
                              <*> ("p2" .: text Nothing)
        fst' (p1, p2) | p1 == p2  = Success p1
                      | otherwise = Error "Passwords must match"

loginForm :: Form Html RaskellHandler AuthUser
loginForm =
  validateM doLogin $ (,) <$> "username" .: text Nothing
                          <*> "password" .: text Nothing
  where doLogin (u, p) = do
          authRes <- with auth $ loginByUsername (encodeUtf8 u)
             (ClearText $ encodeUtf8 p) False
          return $ case authRes of
            Left UserNotFound -> Error "User not found"
            Left IncorrectPassword -> Error "Incorrect password"
            Left PasswordMissing -> Error "Password missing"
            Left (LockedOut _) -> Error "This account is disabled"
            Left _ -> Error "Unknown error"
            Right u' -> Success u'
