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
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- ****************************************************************************************

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.

-- RECUPERADO DE LABORATORIO 04:

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



-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.

desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] alternativa = desugar alternativa
desugarCond ((condicion, rama) : resto) alternativa = do
  c <- desugar condicion
  r <- desugar rama
  a <- desugarCond resto alternativa
  return (If c r a)



-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.

 
desugar :: SASA -> Maybe ASA

desugar (IdS x)      = Just (Id x)
desugar (NumS n)     = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)

desugar (AddS es) = binaryOp Add =<< mapM desugar es
desugar (SubS es) = binaryOp Sub =<< mapM desugar es
desugar (NotS e)  = Not <$> desugar e

desugar (FunS ps cuerpo) = do
  cuerpo' <- desugar cuerpo
  curryFun ps cuerpo'

desugar (AppS f args) = do
  f'    <- desugar f
  args' <- mapM desugar args
  curryApp f' args'

desugar (LetS x e cuerpo) = do
  e'      <- desugar e
  cuerpo' <- desugar cuerpo
  return (App (Fun x cuerpo') e')

desugar (LetStarS bindings cuerpo) = do
  cuerpo' <- desugar cuerpo
  foldr (\(x, e) acc -> do
            e' <- desugar e
            return (App (Fun x acc) e'))
        (Just cuerpo')
        bindings

desugar (IfS c t e) = do
  c' <- desugar c
  t' <- desugar t
  e' <- desugar e
  return (If c' t' e')



desugar (CondS clausulas alternativa) =
  desugarCond clausulas alternativa


desugar (LetRecS f definicion cuerpo) =
  desugar
    (LetS f
          (AppS (IdS "Y") (FunS [f] definicion))
          cuerpo)
-- ****************************************************************************************


-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.

lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((y, v) : resto)
  | x == y    = Just v
  | otherwise = lookupEnv x resto

  
strict :: Value -> Maybe Value
strict (ExprV e env) = bigStep env e >>= strict
strict v = Just v

asNum :: Value -> Maybe Int
asNum (NumV n) = Just n
asNum _ = Nothing
 
asBool :: Value -> Maybe Bool
asBool (BooleanV b) = Just b
asBool _ = Nothing
 
strictNum :: Env -> ASA -> Maybe Int
strictNum env e = bigStep env e >>= strict >>= asNum
 
strictBool :: Env -> ASA -> Maybe Bool
strictBool env e = bigStep env e >>= strict >>= asBool
 
-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
applyClosure :: Env -> ASA -> Value -> Maybe Value
applyClosure env arg (ClosureV p cuerpo envF) =
  bigStep ((p, ExprV arg env) : envF) cuerpo
applyClosure _ _ _ = Nothing

selectBranch :: Env -> ASA -> ASA -> Bool -> Maybe Value
selectBranch env t _ True  = bigStep env t
selectBranch env _ e False = bigStep env e
 

bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x)      = lookupEnv x env
bigStep _   (Num n)     = Just (NumV n)
bigStep _   (Boolean b) = Just (BooleanV b)
bigStep env (Fun p b)   = Just (ClosureV p b env)
bigStep env (Add l r)   =
  (\a b -> NumV (a + b)) <$> strictNum env l <*> strictNum env r
bigStep env (Sub l r)   =
  (\a b -> NumV (max 0 (a - b))) <$> strictNum env l <*> strictNum env r
bigStep env (Not e)     = (BooleanV . not) <$> strictBool env e
bigStep env (If c t e)  = strictBool env c >>= selectBranch env t e
bigStep env (App f a)   = bigStep env f >>= strict >>= applyClosure env a