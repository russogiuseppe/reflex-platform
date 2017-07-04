{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE JavaScriptFFI             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Typeable
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
import           Prelude                        hiding ((!!), id) 

data Quote = Quote
  { text   :: String
  , author :: String
  , id :: Int 
  , isSticky :: Bool
  } deriving (Eq, Show, Read, Generic, GMI.ToJSVal, GMI.FromJSVal, Typeable)

data Request = Request 
  { method :: String,
    body :: String
    }deriving (Eq, Show, Read, Generic, GMI.ToJSVal, GMI.FromJSVal,Typeable)

type Header = TS.JSVal
type Promise = TS.JSVal

foreign import javascript interruptible "setTimeout($c, $1);" delay::
               Int -> IO()          
foreign import javascript unsafe "$1[$2]" getVal' ::
               TS.JSVal -> TS.JSString -> IO TS.JSVal
foreign import javascript unsafe "$1[$2] = $3" setVal ::
               TS.JSVal -> TS.JSString -> TS.JSVal -> IO ()              
foreign import javascript unsafe "$r = {$1 : $2}" js_setHeader ::
               TS.JSString -> TS.JSVal -> IO Header 
foreign import javascript unsafe "$r = window[$1]" readGlobal ::
               TS.JSString -> IO TS.JSVal
foreign import javascript unsafe "window[$1] = $2" writeGlobalFunction ::
               TS.JSString -> Callback (TS.JSVal -> IO ()) -> IO ()
foreign import javascript unsafe "JSON.stringify($1)" js_encode ::
               TS.JSVal -> TS.JSString
foreign import javascript safe "$r = window[$1]($2)" invoke1 ::
               JSString -> TS.JSVal -> IO TS.JSVal
foreign import javascript safe "$r = window[$1]($2,$3)" invoke2 ::
               TS.JSString -> TS.JSVal -> TS.JSVal -> IO TS.JSVal
foreign import javascript unsafe "$r = $1[$2]()" callm1 ::
                TS.JSVal -> TS.JSString -> IO TS.JSVal
foreign import javascript unsafe "$1[$2]($3)" callm1WithVal :: 
                TS.JSVal -> TS.JSString -> TS.JSString -> IO ()
foreign import javascript unsafe "$r = $1.then($2)" js_then' ::
                Promise -> Callback (TS.JSVal -> IO Promise)  -> IO Promise
foreign import javascript unsafe "$r = $1.then($2)" js_then ::
                Promise -> Callback (TS.JSVal -> IO ())  -> IO Promise

returnPromiseJson = (syncCallback1' $ \r -> do
                    v <- callm1 r (T.toJSString "json")
                    return v)

fetch :: String -> IO Promise
fetch x = do
  myValue <- GMI.toJSVal x
  invoke1 (T.toJSString "fetch") myValue

fetch' :: String -> TS.JSVal -> IO Promise 
fetch' x y = do
  myValue <- GMI.toJSVal x  
  invoke2 (T.toJSString "fetch") myValue y

myGetJSON' :: String -> TS.JSVal -> IO Promise 
myGetJSON' url y = do
  o <- fetch' url y  
  cb <- returnPromiseJson
  js_then' o cb

myGetJSON :: String -> IO Promise
myGetJSON url = do
  o <- fetch url
  cb <- returnPromiseJson
  js_then' o cb

endpoint :: String
endpoint = "api/quotations"

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
          Nothing  -> error "Invalid id specified"
      getQuoteFromPage d = do
        newQuote <- getTextValueWithId d "new-quote"
        author <- getTextValueWithId d "quote-author" 
        if author == [] then return $ Quote newQuote "Anonymous" 0 False
        else return $ Quote newQuote author 0 False
      createRowFromQuote d q = do
        Just e <- D.createElement d (Just "tr")
        E.setId e (show (id q))
        if(isSticky q == True) then do{
          E.setInnerHTML e $ 
            Just $
            "<td>" ++ text q ++ "</td><td>" ++ "by " ++ author q ++ "</td>";
          return e;
        }else do{        
          E.setInnerHTML e $
            Just $
            "<td>" ++ text q ++
            "</td><td>" ++
            "by " ++ author q ++
            "</td><button onClick=" ++ "myHandler" ++ "(" ++ show (id q) ++ ")" ++ ">Delete</button>";
          return e;  }
      addRowToTable d r = do
        qt <- uGetById d "quotations"
        _ <- NE.appendChild qt (Just r)
        return ()
      setFunction idNum doc = do 
        (Just (idn :: Int)) <- GMI.fromJSVal idNum
        request <- GMI.toJSVal (Request "DELETE" "")
        o <- fetch' (endpoint ++ "/" ++ show (idn)) request
        cb <- (asyncCallback1 $ \_ -> do{
          qut <- uGetById doc (show (idn));
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
      loadQuotations d url = do {
  		  val <- myGetJSON url;
 		    cb <- (asyncCallback1 $ \res -> do 
                	(Just (array :: [Quote])) <- GMI.fromJSVal res 
                	mapM_ (\x -> do {
                            r <- createRowFromQuote d x; addRowToTable d r; return();
                  }) array
                	return () );
  		  _ <- js_then val cb ;
  		  return(); }
      addQuote doc q = do{
        quote <- (GMI.toJSVal q);
        apjson <- GMI.toJSVal "application/json";
        header <- js_setHeader (T.toJSString "content-type") apjson;
        rq <- GMI.toJSVal (Request "POST" (DJS.unpack (js_encode quote)));
        setVal rq (T.toJSString "headers") header;
        val <- myGetJSON' endpoint rq;
        cb <- (asyncCallback1 $ \res -> do{
                      (Just (qt::Quote)) <- GMI.fromJSVal res;
                      r <- createRowFromQuote doc qt;
                      addRowToTable doc r;
                      return();
          });
        _ <- js_then val cb;
        return();
      }
      registerServiceWorker2 doc url = do{
        nav <- liftIO $ readGlobal (T.toJSString "navigator");
        sw <- liftIO $ getVal' nav (T.toJSString "serviceWorker");
        ctrl <- liftIO $ getVal' sw (T.toJSString "controller");
        if(TS.isNull ctrl) then do{
          nav1 <- liftIO $ readGlobal (T.toJSString "navigator");
          sw1 <- getVal' nav1 (T.toJSString "serviceWorker"); 
          callm1WithVal sw1 (T.toJSString "register") (T.toJSString "service-worker.js");
          delay 500;
          loadQuotations doc url;
          return(); 
        }else do{
          loadQuotations doc url;
          return();
        }
      }
  in R.runWebGUI $ \webView -> do
       Just doc <- R.webViewGetDomDocument webView
       Just myForm <- gGetById FE.castToHTMLFormElement doc "add-form"
       deleteQuote <- asyncCallback1 $ \idNum -> setFunction idNum doc
       writeGlobalFunction (DJS.pack "myHandler") deleteQuote
       registerServiceWorker2 doc endpoint
       void $
         Ev.on myForm E.submit $ do
          Ev.preventDefault
          q <- getQuoteFromPage doc
          if (text q) == [] then return() 
          else do 
            liftIO $ addQuote doc q
            return()