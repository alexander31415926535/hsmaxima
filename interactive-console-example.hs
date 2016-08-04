import Maxima
 
maximaPrompt srv = do
    putStr "> "
    question <- getLine
    if question == ":q"
       then return ()
       else do answer <- askMaxima srv question
               print answer
               maximaPrompt srv
               
main = runMaxima 4424 maximaPrompt
