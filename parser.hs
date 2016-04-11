import Data.Char
import Control.Arrow

main = do
   x <- readFile "entrada.txt"
   let resultado = read x :: Int
   let var=resultado+1
   let lol = show var :: String
   writeFile "saida.txt" (lol)
   print (lol)



