{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class

-- For an up to date list of what ghcjs-dom can do, download and grok:
-- https://hackage.haskell.org/package/ghcjs-dom-0.2.3.1/ghcjs-dom-0.2.3.1.tar.gz
import qualified GHCJS.DOM as R
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.HTMLFormElement as FE
import Prelude hiding ((!!))

endpoint :: String
endpoint = "api/quotations"

data Quote = Quote
  { text :: String
  , author :: String
  } deriving (Eq, Show, Read)

main :: IO ()
main =
  let gGetById f d i = fmap f <$> D.getElementById d i
  in R.runWebGUI $ \webView -> do
       Just doc <- R.webViewGetDomDocument webView
       Just myForm <- gGetById FE.castToHTMLFormElement doc "add-form"
       -- ora che abbiamo il riferimento a myForm, registriamo l'handler del pulsante
       void $
         Ev.on myForm E.submit $
         -- Stampiamo nella console del browser il seguente messaggio
          do liftIO $ putStrLn "Event received!"
       return ()
