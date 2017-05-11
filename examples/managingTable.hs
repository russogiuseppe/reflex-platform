{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE JavaScriptFFI             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Monad

-- For an up to date list of what ghcjs-dom can do, download and grok:
-- https://hackage.haskell.org/package/ghcjs-dom-0.2.3.1/ghcjs-dom-0.2.3.1.tar.gz
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Char
import           Data.JSString                  as DJS
import           GHC.Generics
import qualified GHCJS.DOM                      as R
import qualified GHCJS.DOM.Document             as D
import qualified GHCJS.DOM.Element              as E
import qualified GHCJS.DOM.EventM               as Ev
import qualified GHCJS.DOM.HTMLButtonElement    as BE
import qualified GHCJS.DOM.HTMLFormElement      as FE
import qualified GHCJS.DOM.HTMLInputElement     as IE
import qualified GHCJS.DOM.HTMLTableElement     as TE
import qualified GHCJS.DOM.Node                 as NE
import qualified GHCJS.DOM.Types                as T
import           GHCJS.Foreign.Callback
import qualified GHCJS.Marshal.Internal         as GMI
import qualified GHCJS.Types                    as TS
import           Prelude                        hiding ((!!))

-- Vogliamo convertire Quote automaticamente da/a un JSVal. Per far questo
-- usiamo "deriving ... GMI.ToJSVal, FMI.FromJSVal"
data Quote = Quote
  { quoteText   :: String
  , quoteAuthor :: String
  } deriving (Eq, Show, Read, Generic, GMI.ToJSVal, GMI.FromJSVal)

-- Funzioni ausiliarie per leggere e scrivere un valore globale nel browser
foreign import javascript unsafe "window[$1] = $2" writeGlobal ::
               TS.JSString -> TS.JSVal -> IO ()

foreign import javascript unsafe "$r = window[$1]" readGlobal ::
               TS.JSString -> IO TS.JSVal

foreign import javascript unsafe "window[$1] = $2"
               writeGlobalFunction ::
               TS.JSString -> Callback (TS.JSVal -> IO ()) -> IO ()

-- Queste due funzioni ci permettono di scrivere e leggere una variabile Haskell in una
-- variabile globale javascript
writeID :: Int -> IO ()
writeID num = do
  idToSave <- GMI.toJSVal num
  writeGlobal (DJS.pack "idQuote") idToSave

readID :: IO Int
readID = do
  idNum <- readGlobal (DJS.pack "idQuote")
  (Just (idToSave :: Int)) <- GMI.fromJSVal idNum
  return idToSave

writeQuote :: Quote -> IO ()
writeQuote q = do
  valueToWrite <- GMI.toJSVal q
  writeGlobal (DJS.pack "exampleQuote") valueToWrite

readQuote :: IO Quote
readQuote = do
  valueRead <- readGlobal (DJS.pack "exampleQuote")
  (Just (q :: Quote)) <- GMI.fromJSVal valueRead
  return q

readQuoteArray :: IO [Quote]
readQuoteArray = do
  valueRead <- readGlobal (DJS.pack "arrayQuote")
  (Just (q :: [Quote])) <- GMI.fromJSVal valueRead
  return q

writeQuoteArray :: [Quote] -> IO ()
writeQuoteArray q = do
  arrayQuote <- GMI.toJSVal q
  writeGlobal (DJS.pack "arrayQuote") arrayQuote

main :: IO ()
main =
  let gGetById f d i = fmap f <$> D.getElementById d i
      uGetById d i = do
        Just e <- D.getElementById d i
        return e
      gGetByTagName f d i = fmap f <$> E.getElementsByTagName d i
      getTextValueWithId d i = do
        Just t <- gGetById IE.castToHTMLInputElement d i
        (t' :: Maybe String) <- IE.getValue t
        case t' of
          Just t'' -> return t''
          Nothing  -> error "Invalid id specified"
      getQuoteFromPage d = do
        newQuote <- getTextValueWithId d "new-quote"
        author <- getTextValueWithId d "quote-author"
        return $ Quote newQuote author
      createRowFromQuote d q = do
        Just e <- D.createElement d (Just "tr")
        idNum <- liftIO readID
        E.setId e ("row" ++ (show idNum))
        --liftIO $ writeID $ idNum + 1
        E.setInnerHTML e $
          Just $
          "<td>" ++
          quoteText q ++
          "</td><td>" ++
          quoteAuthor q ++
          "</td><button onClick=" ++ "myHandler" ++ "(" ++ show idNum ++ ")" ++ ">Delete</button>"
        return e
      addRowToTable d r = do
        qt <- uGetById d "quotations"
        _ <- NE.appendChild qt (Just r)
        return ()
    -- si dovrebbe scrivere l'handler che deve riuscire a chiamare la funzione di haskell
    -- per riuscirci dobbiamo capire per bene come funziona la questione della call back
  in R.runWebGUI $ \webView -> do
       Just doc <- R.webViewGetDomDocument webView
       Just myForm <- gGetById FE.castToHTMLFormElement doc "add-form"
       writeQuoteArray []
       deleteQuote <- asyncCallback1 $ \idNum -> do { (Just (idn :: Int)) <- GMI.fromJSVal idNum;
                                                      qut <- uGetById doc ("row" ++ show idn); 
                                                      Just table <- NE.getParentNode qut; 
                                                      NE.removeChild table (Just qut); 
                                                      return() }
       writeGlobalFunction (DJS.pack "myHandler") deleteQuote
       void $
         Ev.on myForm E.submit $ do
           Ev.preventDefault
           q <- getQuoteFromPage doc
           qa <- liftIO readQuoteArray
           liftIO $ writeQuoteArray (qa ++ [q])
           r <- createRowFromQuote doc q
           idNum <- liftIO readID
           liftIO $ writeID (idNum + 1)
           addRowToTable doc r
       return ()
