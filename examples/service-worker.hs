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
import           Prelude                        hiding ((!!), id)
import qualified GHCJS.DOM.XMLHttpRequest       as XM 
import qualified JavaScript.Object.Internal     as O 
import qualified GHCJS.DOM.Location             as LOC


foreign import javaScript unsafe "importScripts($1)" importScripts ::
			TS.JSString -> IO ()
foreign import javaScript unsafe "$r = new ServiceWorkerWare" createWorker::
			IO TS.JSVal 

data Quote = Quote
  { text   :: String
  , author :: String
  , id :: Int 
  , isSticky :: Bool
  } deriving (Eq, Show, Read, Generic, GMI.ToJSVal, GMI.FromJSVal, Typeable)

quotations :: [Quote]
quotations = ["Humanity is smart. Sometime in the technology world we think we are smarter, but we are not smarter than you." "Mitchell Baker" 1 True, 
			  "A computer would deserve to be called intelligent if it could deceive a human into believing that it was human." "Alan Turing" 2 True,
			  "If you optimize everything, you will always be unhappy" "Donald Knuth" 3 True,
			  "If you don't fail at least 90 percent of the time' you' re not aiming high enough" "Alan Key" 4 True
			  "Colorless green ideas sleep furiously." "Naom Chomsky" 5 True]


main :: IO()
main = do
	R.runWebGUI $ \webView -> do
		Just doc <- R.webViewGetDomDocument webView
		importScripts (T.toJSString "./lib/ServiceWorkerWare.js")
		quotes <- GMI.toJSVal quotations
