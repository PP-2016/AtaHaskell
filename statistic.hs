module Statistic (
    mediaAritimetica,
    --desvioPadrao,
    variancia,
    --erroMedia,
    --erroPadrao,
    --minQuadrado
    ) where

mediaAritimetica :: [Float] -> Float
mediaAritimetica xs = sum xs/ (fromIntegral $ length xs)


some :: [Float]-> Float
some [] = 0
some xs = head(xs)-mediaAritimetica xs

--variancia :: [Float] -> [Float]
variancia xs = map some xs