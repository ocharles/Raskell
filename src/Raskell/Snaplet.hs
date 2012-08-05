{-# LANGUAGE TemplateHaskell #-}
module Raskell.Snaplet where

import Data.Lens.Template
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.PostgresqlSimple

data Raskell = Raskell
    { _db :: Snaplet Postgres
    , _session :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager Raskell)
    , rng :: RNG
    }

makeLenses [''Raskell]

type RaskellHandler = Handler Raskell Raskell
