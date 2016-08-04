import Maxima

main = runMaxima 4424 $ \x -> askMaxima x " solve(x*x-3*x+15=0,x) " >>= print
