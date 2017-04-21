module Main where

type Text = String

type Author = String

type Quote = (Text, Author)



loadQuotations :: [Quote] -> Quote -> [Quote]
loadQuotations xs x = xs ++ [x]

main :: IO()
main = do
	return()

	


