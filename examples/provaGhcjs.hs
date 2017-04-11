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

ENDPOINT :: [(String)]
ENDPOINT = "api/quotations"

quotations :: [(t, )]

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
  runWebGUI $ \webWiew -> do
  	getElementById doc "add-form" ^. onsubmit <- do
  		Just doc <- webViewGetDomDocument webView --prendo codice HTML
    	Just body <- documentGetBody doc --prendo solo il body del codice HTML

    	preventDefault
    	let value = jsg "value"

    	newQuote <- getElementById doc "new-quote" ^.value ^. trim --non capisco il significato di .value e non riesco a capire come tradurlo in haskell
    	{-Just newQuote <- fmap castToHTMLInputElement <$> getElementById doc "new-quote"-}
    	if null newQuote then return() else do
    		quoteAuthor <- getElementById doc "quote-author" ^. value ^. trim
    		{-Just quoteAuthor <- fmap castToHTMLInputElement <$> getElementById doc "quote-author"-}
    		if null quoteAuthor then quoteAuthor = "Anonymous" else do
    			let quote = (newQuote, quoteAuthor) --non saprei come gestirla e che tipo assegnare a questa variabile. Dovrei definire il tipo quotations??
    				headers = [("content-type", "application/json")]


    	callBack <- jsg "callbackToHaskell" <# fun (\f this[addedQuote] ->
    		getElementById doc "quotations" ^.
    			getRowFor <- jsg "getRowFor"
    			getRowFor ^. nodeAppendChild

			resizeIframe) --che tipo di variabile è addedQuote? Come gestisco le funzioni del tipo function(addedQuote){...}
    					  --questa parte del codice corrisponde a ciò che c'è nella fetch in function(addedQuote){...}

    	--come gestisco la documentGetElementById.value.trim(). Se la trim è un modificatore di stringa, come faccio a passargli la stringa contenuta nel value? Cosa fa la .value????'
    	--non capisco cosa faccia la headers ={'content-type': 'application/json'} non ho minimamente idea ad una traduzione in ghcjs
    	--Come gestisco la fetch di serviceWorkerWare??????? Come potrei implementarla in haskell. Inoltre la fetch prende in ingresso un ENDPOINT e una funzione e in un altro caso solo l'ENDPOINT
    	--non mi sono ancora chiare bene le js1, js4 e jsg. Cosa fanno, come agiscono? A prima vista mi sembra prendano una stringa in ingresso e la traducano in funzione javascript. Sarebbe un qualcosa di troppo magico se fosse vero
    	--dato che ghcjs crea un html tutto suo, devo assegnare prima tutto l'html prima di iniziare con la definizione del codice??


    	getCell :: Text -> IO(Maybe Element)
    	getCell v  = do
    		td <- createElement doc "TD"
    		td ^. getTextContent <- v

