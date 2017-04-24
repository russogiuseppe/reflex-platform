{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class

-- For an up to date list of what ghcjs-dom can do, download and grok:
-- https://hackage.haskell.org/package/ghcjs-dom-0.2.3.1/ghcjs-dom-0.2.3.1.tar.gz
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char
import Control.Monad
import qualified GHCJS.DOM as R
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.HTMLFormElement as FE
import Prelude hiding ((!!))
import qualified GHCJS.DOM.HTMLInputElement as IE 
import qualified GHCJS.DOM.HTMLTableElement as TE 
import qualified GHCJS.DOM.JSFFI.Generated.Node as NE 

{-endpoint :: String
endpoint = "api/quotations"-}

{-data Quote = Quote
  { text :: String
  , author :: String
  } deriving (Eq, Show, Read) -}
type StateType = Int
type StateMonad a = StateT StateType IO a

increment :: StateMonad()
increment = do{
          modify(+1);
          return();
        }

rif :: Int
rif = 1


main :: IO ()
main =
  let gGetById f d i = fmap f <$> D.getElementById d i
      cCreateById m n o = fmap m <$> D.createElement n o  
  in R.runWebGUI $ \webView -> do
       Just doc <- R.webViewGetDomDocument webView
       Just myForm <- gGetById FE.castToHTMLFormElement doc "add-form"
       -- ora che abbiamo il riferimento a myForm, registriamo l'handler del pulsante
       void $
         Ev.on myForm E.submit $
         -- Stampiamo nella console del browser il seguente messaggio
          do
            Ev.preventDefault

            Just newQuote <- gGetById IE.castToHTMLInputElement doc "new-quote"
            quote <- IE.getValue newQuote
            Just quoteAuthor <- gGetById IE.castToHTMLInputElement doc "quote-author"
            author <- IE.getValue newQuote

            --dopo aver preso i riferimenti e le stringhe dei valori inseriti dall'utente, creo le due celle con le stringhe
            Just td1 <- cCreateById NE.castToNode doc "TD"
            setTextContent td1 quote 
            Just td2 <- cCreateById NE.castToNode doc "TD"
            setTextContent td2 ("by" ++ author)

            --creiamo l'elemento tr e ad esso leghiamo i due elementi td
            Just tr <- cCreateById NE.castToNode doc "TR"
            E.setId tr (\rif -> show rif)
            runStateT increment rif
            NE.appendChild tr td1
            NE.appendChild tr td2

            --aggiungiamo il nodo tr a tabella
            Just quotations <- gGetById TE.castToHTMLTableElement doc "quotations"
            NE.appendChild quotations tr 
       return ()
