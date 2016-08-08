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
tounicode string = foldl1 (.) (zipWith (\x y -> replace x y) ("*":terms) ("·":helper terms)) string
  where terms = case parseOnly allpowers (pack string) of
                 Left _     -> []
                 Right pstr -> pstr

        helper xp = map (foldl1 (.) (zipWith (\x y -> replace x y) ["^","1","2","3","4","5","6","7","8","9","0"]
                                                                   ["","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹","⁰"])) xp


(<^>) = flip (<?>)              -- more convenient help combinator

powerp :: Parser String
powerp = "Powerp"  <^> ((:) <$> char '^' <*> (many1 digit))

allpowers :: Parser [String]
allpowers = "allpowers"  <^> many' (takeTill (\x -> x == '^') *> powerp)
         
main = runMaxima 4424 maximaPrompt
