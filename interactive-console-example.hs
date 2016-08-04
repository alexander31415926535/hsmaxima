{-# LANGUAGE MultiWayIf #-}

import Maxima
 
maximaPrompt srv = do
    putStr "> "
    question <- getLine
    if
      | question == ":q" -> return ()
      | question == ":h" ->
        do putStrLn "Type maxima command or :q to quit."
           maximaPrompt srv
      | otherwise -> do answer <- askMaxima srv question
                        print answer
                        maximaPrompt srv
               
main = runMaxima 4424 maximaPrompt
