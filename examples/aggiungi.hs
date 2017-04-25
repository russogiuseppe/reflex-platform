module Main where{-questa funzione svolge il semplice ruolo di aggiungere un aforisma ad una lista di aforismi presenti
ed è la classica funzione Haskelle che vogliamo implementare in GHCJS -}

import Language.Javascript.JSaddle

-- data Quotation = Quotation {author :: String,
--                             quotation :: String
--                             } deriving (Show)
{-
aggiungi :: [Quotation] -> Quotation -> [Quotation]
aggiungi xs x = xs ++ [x] -}


-- l'idea è quella di chiamare la funzione javascript concat



-- quotations :: [Quotation]
-- quotations = Quotation{author = "Seneca", quotation = "Per aspera ad Astra"}

-- quote :: [Quotation]
-- quote = Quotation{author = "Cesare", quotation = "Quoque tu, brute fili mi"}

{-va bene JSArray per un array di quotation??-}

-- aggiungi :: JSArray m -> JSValue m -> JSM (JSArray m1)
-- let concat = js1 "concat"
-- in aggiungi  x y  = x ^. concat y

{-main = do
    newQuote <- aggiungi quotations quote
    print (newQuote)
    return() -}

main = putStrLn "Work in progress"
