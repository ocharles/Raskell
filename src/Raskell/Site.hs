{-# LANGUAGE OverloadedStrings #-}
module Raskell.Site where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class (gets)
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Raskell.Database as DB
import           Raskell.Forms
import           Raskell.JSON ()
import           Raskell.Snaplet (RaskellHandler, db, auth, rng)
import           Raskell.Types
import qualified Raskell.View as V
import           Snap.Blaze
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple (commit)
import           Snap.Snaplet.Session.Common
import           Snap.Snaplet.Auth
import           Text.Digestive.Snap
import qualified Text.Hastache as Hastache
import           Text.Hastache.Context (mkStrContext)

textParam :: ByteString -> RaskellHandler (Maybe Text.Text)
textParam = fmap (fmap decodeUtf8) . getParam

toggleRating :: RaskellHandler ()
toggleRating = do
  key' <- textParam "user"
  case key' of
    Just key -> do
      login' <- with db $ DB.resolveKeyToName key
      case login' of
        Just lName -> do
          project' <- textParam "project"
          case project' of
            Just project -> do
              writeJSON =<< (with db $ DB.toggleRating (Rating lName project))
              with db $ commit
            _ -> errorInvalid
        _ -> errorInvalid
    _ -> errorInvalid

errorInvalid :: RaskellHandler ()
errorInvalid = do
  modifyResponse (setResponseCode 400)
  writeText "Invalid request"

userOverview :: RaskellHandler ()
userOverview = do
  user <- textParam "user"
  case user of
    Just u ->
      writeJSON =<< (with db $ DB.userOverview u)
    Nothing -> errorInvalid

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON j = do
  modifyResponse (setContentType "application/json")
  writeLBS $ encode j

register :: RaskellHandler ()
register = do
  (view, result) <- runForm "register" registrationForm
  case result of
    Just (Registration u p) -> do
      newUser <- with auth $ createUser u (encodeUtf8 p) >>= forceLogin
      case newUser of
        Left e -> error . ("Failed" ++) . show $ e
        Right u' -> do
          gets rng >>= liftIO . mkCSRFToken >>=
            with db . DB.addKey (userLogin u') . Text.unpack
          with db $ commit
          redirectToUser u'
    Nothing -> blaze $ V.register view
  where redirectToUser u =
          redirect $ (encodeUtf8 $ Text.append "/user/" $ userLogin u)

login :: RaskellHandler ()
login = do
  (view, result) <- runForm "login" loginForm
  case result of
    Just _ -> redirect "/"
    Nothing -> blaze $ V.login view

installRaskell :: RaskellHandler ()
installRaskell = do
  au' <- with auth currentUser
  case au' of
    Just user -> serveRaskellFor user
    Nothing -> do
      modifyResponse (setResponseCode 404)
      (view, result) <- runForm "login" loginForm
      case result of
        Just user -> serveRaskellFor user
        Nothing -> blaze $ V.login view
  where context h "raskell-hash" = Hastache.MuVariable h
        context _ _ = Hastache.MuNothing

        serveRaskellFor user = do
          h <- userHash user
          writeLBS =<<
            Hastache.hastacheFile Hastache.defaultConfig
              "userscript/raskell.user.js" (mkStrContext $ context h)

        userHash :: AuthUser -> RaskellHandler String
        userHash u = with db $ DB.userKey $ userLogin u

landing :: RaskellHandler()
landing = with auth currentUser >>= blaze . V.landing
