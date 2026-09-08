module Interp where

import Grammars
import Control.Monad (foldM)

-- *****************************************************

-- RETO 3: sustitucion nominal que evita captura

-- *************

freeVars :: ASA -> [String]
freeVars = sinRepetir . fv
  where
    fv (Id x) = [x]
    fv (Num _) = []
    fv (Boolean _) = []
    fv (Add es) = concatMap fv es
    fv (Sub es) = concatMap fv es
    fv (Mul es) = concatMap fv es
    fv (Div es) = concatMap fv es
    fv (And es) = concatMap fv es
    fv (Or es) = concatMap fv es
    fv (Lt es) = concatMap fv es
    fv (Gt es) = concatMap fv es
    fv (Le es) = concatMap fv es
    fv (Ge es) = concatMap fv es
    fv (Expt a b) = fv a ++ fv b
    fv (EqP a b) = fv a ++ fv b
    fv (Not a) = fv a
    fv (Add1 a) = fv a
    fv (Sub1 a) = fv a
    fv (ZeroP a) = fv a
    fv (Let ligas cuerpo) =
      let ligadas = map fst ligas
      in concatMap (fv . snd) ligas ++ filter (`notElem` ligadas) (fv cuerpo)
    fv (LetStar [] cuerpo) = fv cuerpo
    fv (LetStar ((x,e):rest) cuerpo) =
      fv e ++ filter (/= x) (fv (LetStar rest cuerpo))



-- **************

names :: ASA -> [String]
names = sinRepetir . nom
  where
    nom (Id x) = [x]
    nom (Num _) = []
    nom (Boolean _) = []
    nom (Add es) = concatMap nom es
    nom (Sub es) = concatMap nom es
    nom (Mul es) = concatMap nom es
    nom (Div es) = concatMap nom es
    nom (And es) = concatMap nom es
    nom (Or es) = concatMap nom es
    nom (Lt es) = concatMap nom es
    nom (Gt es) = concatMap nom es
    nom (Le es) = concatMap nom es
    nom (Ge es) = concatMap nom es
    nom (Expt a b) = nom a ++ nom b
    nom (EqP a b) = nom a ++ nom b
    nom (Not a) = nom a
    nom (Add1 a) = nom a
    nom (Sub1 a) = nom a
    nom (ZeroP a) = nom a
    nom (Let ligas cuerpo) =
      map fst ligas ++ concatMap (nom . snd) ligas ++ nom cuerpo
    nom (LetStar ligas cuerpo) =
      map fst ligas ++ concatMap (nom . snd) ligas ++ nom cuerpo



-- ********************

freshName :: [String] -> String
freshName usados = head $ filter (`notElem` usados) (base : map (\n -> base ++ show n) [0..])
  where base = "x"




-- ********************

sust :: ASA -> String -> ASA -> ASA
sust expr var valor = aux expr
  where
    aux (Id x) = if x == var then valor else Id x
    aux (Num n) = Num n
    aux (Boolean b) = Boolean b
    aux (Add es) = Add (map aux es)
    aux (Sub es) = Sub (map aux es)
    aux (Mul es) = Mul (map aux es)
    aux (Div es) = Div (map aux es)
    aux (And es) = And (map aux es)
    aux (Or es) = Or (map aux es)
    aux (Lt es) = Lt (map aux es)
    aux (Gt es) = Gt (map aux es)
    aux (Le es) = Le (map aux es)
    aux (Ge es) = Ge (map aux es)
    aux (Expt a b) = Expt (aux a) (aux b)
    aux (EqP a b) = EqP (aux a) (aux b)
    aux (Not a) = Not (aux a)
    aux (Add1 a) = Add1 (aux a)
    aux (Sub1 a) = Sub1 (aux a)
    aux (ZeroP a) = ZeroP (aux a)

    aux (Let ligas cuerpo) =
      let xs = map fst ligas
          es' = map (\(x, e) -> (x, aux e)) ligas
      in if var `elem` xs
         then Let es' cuerpo
         else let libresVal = freeVars valor
                  usados = names (Let ligas cuerpo) ++ libresVal
                  (renombres, nuevasLigas) = foldr (\(y, e) (rens, lgs) ->
                    if y `elem` libresVal
                    then let z = freshName (usados ++ map snd rens)
                         in ((y, z) : rens, (z, e) : lgs)
                    else (rens, (y, e) : lgs)
                    ) ([], []) es'
                  cuerpo' = foldl (\c (old, new) -> sust c old (Id new)) cuerpo renombres
              in Let nuevasLigas (aux cuerpo')

    aux (LetStar [] cuerpo) = LetStar [] (aux cuerpo)
    aux (LetStar ((x, e) : rest) cuerpo) =
      let e' = aux e
      in if x == var
         then LetStar ((x, e') : rest) cuerpo
         else let libresVal = freeVars valor
                  usados = names (LetStar ((x, e) : rest) cuerpo) ++ libresVal
              in if x `elem` libresVal
                 then let z = freshName usados
                          restAndBody' = sust (LetStar rest cuerpo) x (Id z)
                      in case aux restAndBody' of
                           LetStar rest' cuerpo' -> LetStar ((z, e') : rest') cuerpo'
                           _ -> LetStar [(z, e')] (aux restAndBody')
                 else case aux (LetStar rest cuerpo) of
                        LetStar rest' cuerpo' -> LetStar ((x, e') : rest') cuerpo'
                        _ -> LetStar [(x, e')] (aux (LetStar rest cuerpo))


-- **************

sustMany :: ASA -> [Binding] -> ASA
sustMany expr ligas = aux expr
  where
    buscar _ [] = Nothing
    buscar x ((y,v):bs) = if x == y then Just v else buscar x bs

    aux (Id x) = case buscar x ligas of Just v -> v; Nothing -> Id x
    aux (Num n) = Num n
    aux (Boolean b) = Boolean b
    aux (Add es) = Add (map aux es)
    aux (Sub es) = Sub (map aux es)
    aux (Mul es) = Mul (map aux es)
    aux (Div es) = Div (map aux es)
    aux (And es) = And (map aux es)
    aux (Or es) = Or (map aux es)
    aux (Lt es) = Lt (map aux es)
    aux (Gt es) = Gt (map aux es)
    aux (Le es) = Le (map aux es)
    aux (Ge es) = Ge (map aux es)
    aux (Expt a b) = Expt (aux a) (aux b)
    aux (EqP a b) = EqP (aux a) (aux b)
    aux (Not a) = Not (aux a)
    aux (Add1 a) = Add1 (aux a)
    aux (Sub1 a) = Sub1 (aux a)
    aux (ZeroP a) = ZeroP (aux a)

    aux (Let ligasInt cuerpo) =
      let ligadas = map fst ligasInt
          ligasInt' = map (\(y,e) -> (y, aux e)) ligasInt
          ligasExt = filter (\(y,_) -> y `notElem` ligadas) ligas
          cuerpo' = sustMany cuerpo ligasExt
          libresExt = concatMap (freeVars . snd) ligas
          renombres = [(y, freshName (names (Let ligasInt cuerpo) ++ libresExt))
                      | y <- ligadas, y `elem` libresExt]
          cuerpo'' = foldr (\(old,new) acc -> sust acc old (Id new)) cuerpo' renombres
          ligasInt'' = map (\(y,e) -> case lookup y renombres of Just n -> (n,e); Nothing -> (y,e)) ligasInt'
      in Let ligasInt'' cuerpo''


    aux (LetStar ligasInt cuerpo) =
      let go [] b = aux b
          go ((y,e):rest) b =
            let e' = aux e
                rest' = go rest b
                libresExt = concatMap (freeVars . snd) ligas
                rest'' = if y `elem` libresExt
                         then let nuevo = freshName (names (LetStar ligasInt cuerpo) ++ libresExt)
                              in sust rest' y (Id nuevo)
                         else rest'
                ligador = if y `elem` libresExt
                          then let nuevo = freshName (names (LetStar ligasInt cuerpo) ++ libresExt)
                               in (nuevo, e')
                          else (y, e')
            in LetStar [ligador] rest''
      in go ligasInt cuerpo



-- ////////////////////////
sinRepetir :: Eq a => [a] -> [a]
sinRepetir = foldr (\x xs -> if x `elem` xs then xs else x : xs) []

-- ******************************************************

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA

bigStep (Num n) = Just (Num n)

bigStep (Boolean b ) = Just (Boolean b)

bigStep (Id _) = Nothing

bigStep (Add es) = do
  ns <- mapM evalNum es 
  return (Num (sum ns))


bigStep (Mul es) = do
   ns <- mapM evalNum es 
   return (Num (product ns))

bigStep (Sub es) = do
  ns <- mapM evalNum es
  case ns of 
    (n:ns') -> return (Num (foldl truncSub n ns'))
    []      -> Nothing


bigStep (Div es) = do 
  ns <- mapM evalNum es 
  case ns of 
    (n:ns') -> Num <$> foldM validaDiv n ns'
    []      -> Nothing


bigStep(And es) = do
  bs <- mapM evalBool es 
  return (Boolean (and bs))


bigStep (Or es) = do
  bs <- mapM evalBool es
  return (Boolean (or bs))

bigStep (Lt es) = comparadorAux (<) es

bigStep (Gt es) = comparadorAux (>) es

bigStep (Le es) = comparadorAux (<=) es

bigStep (Ge es) = comparadorAux (>=) es

bigStep (Expt a b) = do
  n <- evalNum a
  m <- evalNum b
  return (Num (n ^ m))
 
bigStep (EqP a b) = do
  va <- bigStep a
  vb <- bigStep b
  case (va, vb) of
    (Num n1, Num n2)         -> Just (Boolean (n1 == n2))
    (Boolean b1, Boolean b2) -> Just (Boolean (b1 == b2))
    _                        -> Nothing
 
bigStep (Not e) = do
  v <- bigStep e
  case v of
    Boolean b -> Just (Boolean (not b))
    Num _     -> Just (Boolean False)
    _         -> Nothing
 

bigStep (Add1 e) = do
  n <- evalNum e
  return (Num (n + 1))
 
bigStep (Sub1 e) = do
  n <- evalNum e
  return (Num (truncSub n 1))
 
bigStep (ZeroP e) = do
  n <- evalNum e
  return (Boolean (n == 0))
 

bigStep (Let ligas cuerpo) = do
  let xs = map fst ligas
  if length xs /= length (sinRepetir xs)
    then Nothing
    else do
      vs <- mapM (bigStep . snd) ligas
      bigStep (sustMany cuerpo (zip xs vs))
 

bigStep (LetStar [] cuerpo) = bigStep cuerpo
bigStep (LetStar ((x, e) : resto) cuerpo) = do
  v <- bigStep e
  bigStep (sust (LetStar resto cuerpo) x v)
 


evalNum :: ASA -> Maybe Int
evalNum e = bigStep e >>= asNum
  where 
      asNum (Num n) = Just n
      asNum _       = Nothing

truncSub :: Int -> Int -> Int
truncSub a b = max 0 (a - b)

validaDiv :: Int -> Int -> Maybe Int
validaDiv _ 0 = Nothing
validaDiv a b = Just (a `div` b)

evalBool :: ASA -> Maybe Bool
evalBool e = bigStep e >>= asBool
  where 
    asBool (Boolean b) = Just b
    asBool _           = Nothing


comparadorAux :: (Int -> Int -> Bool) -> [ASA] -> Maybe ASA
comparadorAux op es = do
  ns <- mapM evalNum es
  return (Boolean (and (zipWith op ns (drop 1 ns))))