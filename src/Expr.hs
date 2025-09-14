module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador ( G, Gen, dameUno, muestra, rango95, genNormalConSemilla )
import Histograma ( Histograma, histograma )

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- recrExpr :: ... anotar el tipo ...
recrExpr :: (Float->b) -> (Float->Float->b) -> (b->b->Expr->Expr->b) -> (b->b->Expr->Expr->b) -> (b->b->Expr->Expr->b) -> (b->b->Expr->Expr->b) -> Expr -> b
recrExpr cConst cRango cSuma cResta cMult cDiv e = case e of
  Const x -> cConst x
  Rango a b -> cRango a b
  Suma e1 e2 -> cSuma (rec e1) (rec e2) e1 e2
  Resta e1 e2 -> cResta (rec e1) (rec e2) e1 e2
  Mult e1 e2 -> cMult (rec e1) (rec e2) e1 e2
  Div e1 e2 -> cDiv (rec e1) (rec e2) e1 e2
  where rec = recrExpr cConst cRango cSuma cResta cMult cDiv

-- foldExpr :: ... anotar el tipo ...
foldExpr :: (Float->b) -> (Float->Float->b) -> (b->b->b) -> (b->b->b) -> (b->b->b) -> (b->b->b) -> Expr -> b
foldExpr cConst cRango cSuma cResta cMult cDiv e = case e of
  Const x -> cConst x
  Rango a b -> cRango a b
  Suma e1 e2 -> cSuma (rec e1) (rec e2)
  Resta e1 e2 -> cResta (rec e1) (rec e2)
  Mult e1 e2 -> cMult (rec e1) (rec e2)
  Div e1 e2 -> cDiv (rec e1) (rec e2)
  where rec = foldExpr cConst cRango cSuma cResta cMult cDiv

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval = foldExpr (\x g ->(x, g)) (\a b g -> dameUno (a,b) g) (operacion (+)) (operacion (-)) (operacion (*)) (operacion (/))

operacion :: (Float -> Float -> Float) -> G Float -> G Float -> Gen -> (Float, Gen)
operacion op fl fr g0 = (\(vL, g1) ->(\(vR, g2) -> (op vL vR, g2)) (fr g1)) (fl g0)

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma -- armarHistograma :: Int -> Int -> (Gen -> (Float, Gen)) -> (Gen -> (Histograma, Gen))
armarHistograma m n f g = (\(xs,g1)-> (\(a,b)-> (histograma m (a,b) xs, g1) ) (rango95 xs)) (muestra f n g)

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n e = armarHistograma m n (eval e)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar e = recrExpr show (\a b -> show a ++ "~" ++ show b)
                     (\str1 str2 e1 e2 -> maybeParen (esConstrRecursivoDistintoDe e1 CESuma) str1 ++ " + " ++ maybeParen (esConstrRecursivoDistintoDe e2 CESuma) str2)
                     (\str1 str2 e1 e2 -> maybeParen (esConstrRecursivo e1) str1 ++ " - " ++ maybeParen (esConstrRecursivo e2) str2)
                     (\str1 str2 e1 e2 -> maybeParen (esConstrRecursivoDistintoDe e1 CEMult) str1 ++ " * " ++ maybeParen (esConstrRecursivoDistintoDe e2 CEMult) str2)
                     (\str1 str2 e1 e2 -> maybeParen (esConstrRecursivo e1) str1 ++ " / " ++ maybeParen (esConstrRecursivo e2) str2)
                     e
  where 
    esConstrRecursivo e = constructor e /= CEConst && constructor e /= CERango
    esConstrRecursivoDistintoDe e c = esConstrRecursivo e && constructor e /= c
    
    -- Para los constructores no recursivos usamos la función show dada
    -- En los constructores recursivos diferenciamos los casos de Multiplicación y Suma para no poner paréntesis innecesarios

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
