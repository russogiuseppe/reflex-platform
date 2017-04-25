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
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.HTMLFormElement as FE
import Prelude hiding ((!!))
import qualified GHCJS.DOM.HTMLInputElement as IE 
import qualified GHCJS.DOM.HTMLTableElement as TE 
import qualified GHCJS.DOM.Node as NE

data Quote = Quote { quoteText :: String , quoteAuthor :: String} deriving (Eq, Show, Read)


main :: IO ()
main =
  let gGetById f d i = fmap f <$> D.getElementById d i

      uGetById d i = do
        Just e <- D.getElementById d i
        return e

      getTextValueWithId d i = do
        Just t <- gGetById IE.castToHTMLInputElement d i
        (t' :: Maybe String) <- IE.getValue t
        case t' of
          Just t'' -> return t''
          Nothing -> error "Invalid id specified"

      getQuoteFromPage d = do
        newQuote <- getTextValueWithId d "new-quote"
        author <- getTextValueWithId d "quote-author"
        return $ Quote newQuote author

      createRowFromQuote d q = do
        Just e <- D.createElement d (Just "tr")
        E.setInnerHTML e $ Just $ "<td>" ++ quoteText q ++ "</td><td>" ++ quoteAuthor q ++ "</td>"
        return e

      addRowToTable d r = do
        qt <- uGetById d "quotations"
        _ <- NE.appendChild qt (Just r)
        return ()


  in R.runWebGUI $ \webView -> do
      Just doc <- R.webViewGetDomDocument webView
      Just myForm <- gGetById FE.castToHTMLFormElement doc "add-form"
      void $
         Ev.on myForm E.submit $
          do
            Ev.preventDefault
            q <- getQuoteFromPage doc
            r <- createRowFromQuote doc q
            addRowToTable doc r
      return ()
