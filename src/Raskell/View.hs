{-# LANGUAGE OverloadedStrings #-}
module Raskell.View where

import           Data.Text (Text)
import           Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Blaze.Html5
import           Text.Digestive.View
import Snap.Snaplet.Auth

pageLayout :: Html -> Html
pageLayout c = do
  H.docType
  H.html $ do
    H.head $
      H.title "Raskell"
    H.body c

register :: View Html -> Html
register v = pageLayout $ do
  H.h1 "Register"
  form v "register" $ do
    fieldRow v "username" "User Name:" inputText
    let passwordV = subView "password" v
    fieldRow passwordV "p1" "Password:" inputPassword
    fieldRow passwordV "p2" "Confrim Password:" inputPassword
    H.p $ errorList "password" v
    H.p $
      H.input ! A.type_ "submit" ! A.value "Register"

fieldRow :: View Html -> Text -> Html -> (Text -> View Html -> Html) -> Html
fieldRow v n l r = H.p $ do
  label n v l
  r n v
  errorList n v

login :: View Html -> Html
login v = pageLayout $ do
  H.h1 "Login"
  form v "" $ do
    fieldRow v "username" "User Name:" inputText
    fieldRow v "password" "Password:" inputPassword
    errorList "" v
    H.p $H.input ! A.type_ "submit" ! A.value "Login"

landing :: Maybe AuthUser -> Html
landing user = pageLayout $ do
  H.h1 "Welcome to Raskell!"
  H.p $ do
    "Raskell is a user script that adds community ratings to "
    H.a ! A.href "http://hackage.haskell.org/" $ "Hackage"
    "."

  H.h1 "Get Raskell"
  case user of
    Just _ -> do
      H.p "To start using Raskell, simply install the following user script:"
      H.a ! A.class_ "big-button" ! A.href "/raskell.user.js" $ "Get Raskell"
    Nothing -> do
      H.p "To use Raskell, you will first need to create a Raskell account:"
      H.a ! A.class_ "big-button" ! A.href "/register" $ "Register a Raskell Account"
