module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola n m = potenciaCola' n m 1  
  where
    potenciaCola' :: Int -> Int -> Int -> Int 
    potenciaCola' _ 0 acc = acc 
    potenciaCola' n m acc = potenciaCola' n (m-1) (acc*n)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS n 0 k = k 1 
potenciaCPS n m k = potenciaCPS n (m-1) (\acc -> k (n* acc)

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola = undefined

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS = undefined

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaaux :: [a] -> [a] -> [a]
reversaaux [] ys = ys
reversaaux (x : xs) ys = reversaaux xs (x : ys)

reversaCola :: [a] -> [a]
reversaCola xs = reversaaux xs [] 

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x : xs) k = reversaCPS xs (\ys -> k (ys ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenainverso :: [[a]] -> [a] -> [a]
concatenainverso [] ys = ys
concatenainverso (xs : xss) ys = concatenainverso xss (reversaaux xs ys)

concatenaCola :: [[a]] -> [a]
concatenaCola xss = reversaaux (concatenainverso xss []) []

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : xss) k = concatenaCPS xss (\ys -> k (xs ++ ys)) 

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys


intercalaaux :: [a] -> [a] -> [a] -> [a]
intercalaaux [] ys zs = reversaaux zs ys
intercalaaux xs [] zs = reversaaux zs xs
intercalaaux (x : xs) (y : ys) zs = intercalaaux xs ys (y : x : zs)

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = intercalaaux xs ys []

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x : xs) (y : ys) k =
  intercalaCPS xs ys (\zs -> k (x : y : zs))