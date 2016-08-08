{-# LANGUAGE MultiWayIf #-}

import Data.Attoparsec.ByteString.Char8 
import Control.Applicative ((<|>)) 
import Data.ByteString.Char8 (pack,unpack) 
import Maxima
import Data.List.Extra hiding (any)

  
maximaPrompt srv = do
    putStr "> "
    question <- getLine
    if
      | question == ":q" -> return ()
      | question == ":h" ->
        do putStrLn "Type maxima command or :q to quit."
           maximaPrompt srv
      | otherwise -> do answer <- askMaxima srv question
                        mapM_ putStrLn (map tounicode answer)
                        maximaPrompt srv
               

tounicode :: String -> String
tounicode string = foldl1 (.) (zipWith (\x y -> replace x y) ("*":terms)
                                                              ["·","²","³","⁴","⁵","⁶","⁷","⁸","⁹"]) string
  where terms = case parseOnly allpowers (pack string) of
          Left _     -> []
          Right pstr -> pstr

(<^>) = flip (<?>)              -- more convenient help combinator

powerp :: Parser String
powerp = "Powerp"  <^> ((:) <$> char '^' <*> (many1 digit))

allpowers :: Parser [String]
allpowers = "allpowers"  <^> many' (skipMany (satisfy (notInClass "^1234567890")) *> powerp) 
         
main = runMaxima 4424 maximaPrompt
