{-# LANGUAGE OverloadedStrings #-}
module Raskell where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Raskell.Site
import Raskell.Snaplet (Raskell(..), RaskellHandler, db, session, auth)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Session.Common
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Util.FileServe (serveFile)

routes :: [(ByteString, RaskellHandler ())]
routes = [ ("/ws/toggle-rating", method GET toggleRating <|> method OPTIONS (return ()))
         , ("/ws/user-overview", method GET userOverview)
         , ("/raskell.user.js", installRaskell)
         , ("/raskell.css", serveFile "userscript/raskell.css")
         , ("/register", register)
         , ("/login", login)
         , ("/", landing)
         ]

raskell :: SnapletInit Raskell Raskell
raskell = makeSnaplet "raskell" "Raskell" Nothing $ do
    addRoutes routes
    wrapSite enableCors
    dbS <- nestSnaplet "db" db pgsInit
    sessionS <- nestSnaplet "session" session $
      initCookieSessionManager "cookie_key" "raskell" Nothing
    authS <- nestSnaplet "auth" auth $ initPostgresAuth session dbS
    rng' <- liftIO mkRNG
    return $ Raskell dbS sessionS authS rng'
  where enableCors = (>> modifyResponse (addHeader "Access-Control-Allow-Origin" "*"))

