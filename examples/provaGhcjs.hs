module Main where

import Prelude hiding ((!!))
import Control.Monad.Trans ( liftIO )
import System.IO (stderr, hPutStrLn, stdout, hFlush)
import GHCJS.DOM (runWebGUI, postGUISync, postGUIAsync, webViewGetDomDocument)
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLElement
import Data.Text.Lazy (Text, unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import Text.Blaze.Html (Html)
import GHCJS.DOM.Types
import Control.Applicative ((<$>))
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLInputElement
import Control.Concurrent
import Control.Monad (when, forever)
import GHCJS.DOM.EventM
       (mouseShiftKey, mouseCtrlKey)
import GHCJS.DOM.Node
       (nodeInsertBefore, nodeAppendChild)
import GHCJS.DOM.CSSStyleDeclaration
       (cssStyleDeclarationSetProperty)
import Language.Javascript.JSaddle
       (strToText, valToStr, JSNull(..), deRefVal, valToObject, js, JSF(..), js1, js4, jsg,
        valToNumber, (!), (!!), (#), (<#), global, eval, fun, val, array, new, runJSaddle_,
        valToText, MakeValueRef(..), JSValue(..), call, JSM(..), JSValueRef)
import Control.Monad.Reader (ReaderT(..))
import qualified Data.Text as T (unpack, pack)
import FRP.Sodium
import Engine
import Freecell -- What could this be for ? :-)
import Language.Haskell.TH
       (stringL, litE)
import System.IO.Unsafe (unsafePerformIO)
import Control.Lens ((^.))
import Control.Exception (throwTo, catch, SomeException, Exception)
import Data.Typeable (Typeable)
import Control.DeepSeq (deepseq)
import GHCJS.DOM.EventM (preventDefault)
import JavaScript.JQuery
import GHCJS.DOM.JSFFI.Generated.Node
import GHCJS.DOM.JSFFI.Generated.TreeWalker


endpoint :: String
endpoint = "api/quotations"

{- quotations :: [quote]

quote :: (text, author)

text :: String

author :: String -}

data Quotations = Quotations {
  text :: JSString
  author :: JSString
} deriving (Eq, Show, Read)



main = do
{-	if (navigator.serviceWorker.controller) {
  loadQuotations();
} else {
  navigator.serviceWorker.oncontrollerchange = function() {
    this.controller.onstatechange = function() {
      if (this.state === 'activated') {
        loadQuotations();
      }
    };
  };
  navigator.serviceWorker.register('service-worker.js');
  -}

  {- TENTATIVO DI CODING DELLA PARTE CHE INIZIA DALLA document.getElementById('add-form').onsubmit-}
  runWebGUI $ \webWiew -> do
  	getElementById doc "add-form" ^. onsubmit <- do
  		Just doc <- webViewGetDomDocument webView --prendo codice HTML
    	Just body <- documentGetBody doc --prendo solo il body del codice HTML

    	preventDefault
    	let value = jsg "value"

    	newQuote <- getElementById doc "new-quote" ^. DeRefVal ^. trim --non capisco il significato di .value e non riesco a capire come tradurlo in haskell
    	{-Just newQuote <- fmap castToHTMLInputElement <$> getElementById doc "new-quote"-}
    	if null newQuote then return() else do
    		quoteAuthor <- getElementById doc "quote-author" ^. DeRefVal ^. trim
    		{-Just quoteAuthor <- fmap castToHTMLInputElement <$> getElementById doc "quote-author"-}
    		if null quoteAuthor then quoteAuthor = "Anonymous" else do
    			let quote = Quotations {text = newQuote, author = quoteAuthor} --non saprei come gestirla e che tipo assegnare a questa variabile. Dovrei definire il tipo quotations??
    				  headers = [("content-type", "application/json")]

      {-PROVA DI UNA CALLBACK PRESENTE NELLA SERIE DI FUNZIONI NELL fetch. PRECISAMENTE L'ULTIMA CALLBACK IN ORDINE DI SCRITTURA-}
    	callBack <- jsg "callbackToHaskell" <# fun (\f this[addedQuote] -> --devo inserire quote o addedQuote qui?
    		getElementById doc "quotations" ^.
    			getRowFor <- jsg "getRowFor"
    			getRowFor ^. nodeAppendChild --non capisco perché si passa da una quote ad una addedQuote. Non è dichiarata da nessuna parte la variabile addedQuote

			resizeIframe) --che tipo di variabile è addedQuote? Come gestisco le funzioni del tipo function(addedQuote){...}
    					  --questa parte del codice corrisponde a ciò che c'è nella fetch in function(addedQuote){...}

    	--come gestisco la documentGetElementById.value.trim(). Se la trim è un modificatore di stringa, come faccio a passargli la stringa contenuta nel value? Cosa fa la .value????'
    	--non capisco cosa faccia la headers ={'content-type': 'application/json'} non ho minimamente idea ad una traduzione in ghcjs
    	--Come gestisco la fetch di serviceWorkerWare??????? Come potrei implementarla in haskell. Inoltre la fetch prende in ingresso un ENDPOINT e una funzione e in un altro caso solo l'ENDPOINT
    	--non mi sono ancora chiare bene le js1, js4 e jsg. Cosa fanno, come agiscono? A prima vista mi sembra prendano una stringa in ingresso e la traducano in funzione javascript. Sarebbe un qualcosa di troppo magico se fosse vero
    	--dato che ghcjs crea un html tutto suo, devo assegnare prima tutto l'html prima di iniziare con la definizione del codice??
  }

{-TENTATIVO DI CODING DELLA function getCell(text)-}
getCell :: Text -> IO(Maybe Element) --Non penso sia corretto il tipo Text. Bisogna capire come definire le variabili quotations e quote. Perché penso che nei codici java faccia parte del ServiceWorker
getCell v  = do
    document <- jsg "document"
    createElement <- js1 "createElement"
    td <- createElement "TD"
    td ^. getTextContent <- v
    return(td)

{-TENTATIVO DI CODING DELLA function getDeleteButton(id)-}
getDeleteButton :: elementId -> IO (Maybe Element)
getDeleteButton iden = do
      document <- jsg "document"
      createElement <- js1 "createElement"
      td <- document ^. createElement "TD"
      bt <- document ^. createElement "BUTTON"
      bt ^. getTextContent <- "Delete"

      callBack <- jsg "callbackToHaskell" <# fun (\f this -> do --come si gestiscono più callback in fila??
                document <- jsg "document"
                getElementById <- js1 "getElementById"
                tr <- document ^. getElementById
                tr ^. getParentNode ^. removeChild tr)













