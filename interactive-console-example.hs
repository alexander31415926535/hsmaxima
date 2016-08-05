{-# LANGUAGE MultiWayIf #-}

import Maxima
import Data.List.Extra

  
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
tounicode x = foldl1 (.) (zipWith (\x y -> replace x y) ["^2","^3","^4","^5","^6","^7","^8","^9"]
                                            ["²","³","⁴","⁵","⁶","⁷","⁸","⁹"]) x
   
main = runMaxima 4424 maximaPrompt
