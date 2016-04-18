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


variancia :: [Float] -> Float -> Float
variancia [] = 0
variancia (h:xs) lista=  h-mediaAritimetica lista
