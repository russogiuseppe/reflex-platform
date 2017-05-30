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
import           Data.Typeable
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
import qualified GHCJS.DOM.XMLHttpRequest       as XM 

-- Vogliamo convertire Quote automaticamente da/a un JSVal. Per far questo
-- usiamo "deriving ... GMI.ToJSVal, FMI.FromJSVal"
data Quote = Quote
  { text   :: String
  , author :: String
  , id :: Int 
  , isSticky :: Bool
  } deriving (Eq, Show, Read, Generic, GMI.ToJSVal, GMI.FromJSVal, Typeable)

data Request = Request 
  { method :: TS.JSString,
    body :: TS.JSString,
    headers :: Header
    } deriving (T.IsJSVal,Typeable) 


type Header = TS.JSVal
type Promise = TS.JSVal

-- Funzioni ausiliarie per leggere e scrivere un valore globale nel browser       
foreign import javascript unsafe "$r = {$1 : $2}" js_setHeader ::
               TS.JSString -> TS.JSString -> IO Header 
foreign import javascript unsafe "window[$1] = $2" writeGlobal ::
               TS.JSString -> TS.JSVal -> IO ()

foreign import javascript unsafe "$r = window[$1]" readGlobal ::
               TS.JSString -> IO TS.JSVal

foreign import javascript unsafe "window[$1] = $2"
               writeGlobalFunction ::
               TS.JSString -> Callback (TS.JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "JSON.stringify($1)" js_encode ::
               TS.JSVal -> TS.JSString

foreign import javascript safe "$r = window[$1]($2)" invoke1 ::
               JSString -> TS.JSVal -> IO TS.JSVal

foreign import javascript safe "$r = window[$1]($2,$3)" invoke2 ::
               TS.JSString -> TS.JSVal -> TS.JSVal -> IO TS.JSVal

foreign import javascript unsafe "$r = $1[$2]()" callm1 ::
                TS.JSVal -> TS.JSString -> IO TS.JSVal

foreign import javascript unsafe "$r = $1.then($2)" js_then' ::
                Promise -> Callback (TS.JSVal -> IO Promise)  -> IO Promise

foreign import javascript unsafe "$r = $1.then($2)" js_then ::
                Promise -> Callback (TS.JSVal -> IO ())  -> IO Promise


fetch :: String -> IO Promise
fetch x = do
  myValue <- GMI.toJSVal x
  invoke1 (T.toJSString "fetch") myValue

fetch' :: String -> Request -> IO Promise 
fetch' x y = do
  myValue <- GMI.toJSVal x  
  invoke2 (T.toJSString "fetch") myValue y

myGetJSON' :: String -> Request -> IO Promise 
myGetJSON' url y = do
  o <- fetch' url y 
  cb <- (syncCallback1' $ \r -> do
                v <- callm1 r (T.toJSString "json")
                return v
                )
  js_then' o cb 

myGetJSON :: String -> IO Promise
myGetJSON url = do
  o <- fetch url
  cb <- (syncCallback1' $ \r -> do
                v <- callm1 r (T.toJSString "json")
                return v
                )
  js_then' o cb

-- Queste due funzioni ci permettono di scrivere e leggere una variabile Haskell in una
-- variabile globale javascript
{-writeID :: Int -> IO ()
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

readQuoteArray :: IO [(String, Quote)]
readQuoteArray = do
  valueRead <- readGlobal (DJS.pack "arrayQuote")
  (Just (q :: [(String, Quote)])) <- GMI.fromJSVal valueRead
  return q

writeQuoteArray :: [(String, Quote)] -> IO ()
writeQuoteArray q = do
  arrayQuote <- GMI.toJSVal q
  writeGlobal (DJS.pack "arrayQuote") arrayQuote

readArray :: IO [Quote]
readArray = do 
  valueRead <- readGlobal (DJS.pack "aQuote")
  (Just (q :: [Quote])) <- GMI.fromJSVal valueRead
  return q 

writeArray :: [Quote] -> IO ()
writeArray q = do 
  arrayQuote <- GMI.toJSVal q 
  writeGlobal (DJS.pack "aQuote") arrayQuote 


deleteQuoteArray :: [(String, Quote)] -> String -> [(String, Quote)]
deleteQuoteArray xs el = [x | x <- xs, not (fst x == el)] -}

endpoint :: String
endpoint = "api/quotations"
 

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
        if author == [] then return $ Quote newQuote "Anonymous"
        else return $ Quote newQuote author
      createRowFromQuote d q = do
        Just e <- D.createElement d (Just "tr")
        E.setId e (show (id q))
        E.setInnerHTML e $
          Just $
          "<td>" ++
          text q ++
          "</td><td>" ++
          "by " ++ author q ++
          "</td><button onClick=" ++ "myHandler" ++ "(" ++ show (id q) ++ ")" ++ ">Delete</button>"
        return e
      addRowToTable d r = do
        qt <- uGetById d "quotations"
        _ <- NE.appendChild qt (Just r)
        return ()
      setFunction idNum doc = do 
        (Just (idn :: Int)) <- GMI.fromJSVal idNum
        o <- fetch' (endpoint ++ "/" ++ show(idn)) Request "DELETE"
        cb <- (asyncCallback1 $ \_ -> do{
          qut <- uGetById doc (show idn);
          Just table <- NE.getParentNode qut;
          NE.removeChild table (Just qut);
          return();
          })
        _ <- js_then o cb 
        return();
      showQuotations d q = do
      	Just e <- D.createElement d (Just "tr")
      	E.setId e (show (id q))
      	E.setInnerHTML e $ 
      		Just $
      		"<td>" ++ text q ++ "</td><td>" ++ "by " ++ author q ++ "</td>"
      	return e 
      loadQuotation d url = do {
  		  val <- myGetJSON url;
 		    cb <- (asyncCallback1 $ \res -> do 
                	(Just (array :: [Quote])) <- GMI.fromJSVal res 
                	mapM_ (\x -> do {
                            row <- showQuotations d x; 
                            addRowToTable d row;
                            return ();
                  }) array
                	return () );
  		  _ <- js_then val cb ;
  		  return(); }
      addQuote doc q = do{
        quote <- T.toJSVal q;
        hd <- liftIO $ js_setHeader (GMI.toJSString "content-type") (GMI.toJSString "application/json");
        val <- myGetJson' endpoint (Request "Post" (js_encode q) hd);
        cb <- (asyncCallback1 $ \res -> do{
                      (Just (q::Quote)) <- GMI.fromJSVal;
                      r <- createRowFromQuote doc q;
                      addRowToTable doc r;
                      return();
          });
        _ <- js_then val cb;
        return();
      }
    -- si dovrebbe scrivere l'handler che deve riuscire a chiamare la funzione di haskell
    -- per riuscirci dobbiamo capire per bene come funziona la questione della call back
  in R.runWebGUI $ \webView -> do
       Just doc <- R.webViewGetDomDocument webView
       Just myForm <- gGetById FE.castToHTMLFormElement doc "add-form"
       deleteQuote <- asyncCallback1 $ \idNum -> setFunction idNum doc
       writeGlobalFunction (DJS.pack "myHandler") deleteQuote
       liftIO $ loadQuotation doc endpoint
       void $
         Ev.on myForm E.submit $ do
          Ev.preventDefault
          q <- getQuoteFromPage doc
          if (text q) == [] then return() 
          else do {
          addQuote doc q;
       return (); }
