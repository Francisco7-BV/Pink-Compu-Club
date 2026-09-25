module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x : xs) e
  | x `elem` xs = Nothing 
  | otherwise = case curryFun xs e of
      Just v -> Just (Fun x v)
      Nothing -> Nothing


-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f args = Just (foldl App f args)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:xs) = Just $ foldl op x xs
-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS xs)
  | Just xs' <- traverse desugar xs = binaryOp Add xs'
  | otherwise = Nothing
desugar (SubS xs)
  | Just xs' <- traverse desugar xs = binaryOp Sub xs'
  | otherwise = Nothing
desugar (NotS e)
  | Just e' <- desugar e = Just (Not e')
  | otherwise = Nothing
desugar (LetS x e1 e2)
  | Just e1' <- desugar e1,
    Just e2' <- desugar e2 =
      Just (App (Fun x e2') e1')
  | otherwise = Nothing
desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x, e) : rest) body) =
  desugar (LetS x e (LetStarS rest body))
desugar (FunS params body)
  | Just body' <- desugar body = curryFun params body'
  | otherwise = Nothing
desugar (AppS f args)
  | Just f' <- desugar f,
    Just args' <- traverse desugar args =
      curryApp f' args'
  | otherwise = Nothing

