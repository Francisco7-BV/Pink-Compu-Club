--Barreto Velazquez Frank
--Valdez Altamirano Areli Nataly

module Laboratorio01 where

-- Reto 01

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt (x*x + y*y)

-- Reto 02

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^2) (filter even xs))

-- Reto 03

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

-- Reto 04

varianza2 :: Double -> Double -> Double
varianza2 p q =
    let m = (p + q)/2
    in ( (p-m)^2 + (q-m)^2 ) / 2

-- Reto 05

clasificaTemperatura :: Int -> String
clasificaTemperatura n
    | n <= 0  = "frio extremo"
    | n <= 15 = "frio"
    | n <= 25 = "templado"
    | n <= 35 = "calido"
    | otherwise = "calor extremo"

-- Reto 06

intercala :: a -> [a] -> [a]
intercala _ [] = []
intercala _ [x] = [x]
intercala n (x:xs) = x : n : intercala n xs

-- Reto 07

data Expr = Lit Int | Suma Expr Expr | Producto Expr Expr
    deriving (Show, Eq)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma x y) = evalua x + evalua y
evalua (Producto x y) = evalua x * evalua y
