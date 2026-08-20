--Barreto Velazquez Frank
--Valdez Altamirano Areli Nataly

module Laboratorio01 where

-- Reto 01

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt (x*x + y*y)

-- Reto 03

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

-- Reto 05

clasificaTemperatura :: Int -> String
clasificaTemperatura n
    | n <= 0  = "frio extremo"
    | n <= 10 = "frio"
    | n <= 25 = "templado"
    | n <= 35 = "calido"
    | otherwise = "calor extremo"

-- Reto 07

data Expr
    = Lit Int
    | Suma Expr Expr
    | Producto Expr Expr
    deriving (Eq, Show)
    
evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma x y) = evalua x + evalua y
evalua (Producto x y) = evalua x * evalua y