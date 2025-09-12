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

import Generador
import Histograma

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
recrExpr cConst cRango cSuma cResta cMult cDiv t = case t of
  Const x -> cConst x
  Rango x y -> cRango x y
  Suma r1 r2 -> cSuma (rec r1) (rec r2) r1 r2
  Resta r1 r2 -> cResta (rec r1) (rec r2) r1 r2
  Mult r1 r2 -> cMult (rec r1) (rec r2) r1 r2
  Div r1 r2 -> cDiv (rec r1) (rec r2) r1 r2
  where rec = recrExpr cConst cRango cSuma cResta cMult cDiv



-- foldExpr :: ... anotar el tipo ...
foldExpr :: (Float->b) -> (Float->Float->b) -> (b->b->b) -> (b->b->b) -> (b->b->b) -> (b->b->b) -> Expr -> b
foldExpr cConst cRango cSuma cResta cMult cDiv t = case t of
  Const x -> cConst x
  Rango x y -> cRango x y
  Suma r1 r2 -> cSuma (rec r1) (rec r2)
  Resta r1 r2 -> cResta (rec r1) (rec r2)
  Mult r1 r2 -> cMult (rec r1) (rec r2)
  Div r1 r2 -> cDiv (rec r1) (rec r2)
  where rec = foldExpr cConst cRango cSuma cResta cMult cDiv

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float -- eval :: Expr → Gen → (Float, Gen)
eval = foldExpr (\x g ->(x, g)) (\a b g -> dameUno (a,b) g) (operacion (+)) (operacion (-)) (operacion (*)) (operacion (*))


operacion :: (Float -> Float -> Float) -> (Gen -> (Float, Gen)) -> (Gen -> (Float, Gen))-> (Gen -> (Float, Gen))
operacion op fl fr g0 = (\(vL, g1) ->(\(vR, g2) -> (op vL vR, g2)) (fr g1)) (fl g0)


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma -- armarHistograma :: Int -> Int -> (Gen -> (Float, Gen)) -> (Gen -> (Histograma, Gen))
armarHistograma m n f g = (\(xs,g1)-> (\(a,b)-> histograma m (a,b) xs, g1 ) (rango95 xs)) (muestra f n g)



-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar e = recrExpr show (\a b -> show a ++ "~" ++ show b)
                     (\str1 str2 e1 e2 -> maybeParen (esConstrRecursivoDistintoDe e1 CESuma) str1 ++ " + " ++ maybeParen (esConstrRecursivoDistintoDe e2 CESuma) str2)
                     (\str1 str2 e1 e2 -> maybeParen (esConstrRecursivo e1) str1 ++ " - " ++ maybeParen (esConstrRecursivo e2) str2)
                     (\str1 str2 e1 e2 -> maybeParen (esConstrRecursivoDistintoDe e1 CEMult) str1 ++ " * " ++ maybeParen (esConstrRecursivoDistintoDe e2 CEMult) str2)
                     (\str1 str2 e1 e2 -> maybeParen (esConstrRecursivo e1) str1 ++ " / " ++ maybeParen (esConstrRecursivo e2) str2) e
  where 
    esConstrRecursivo e = constructor e /= CEConst && constructor e /= CERango
    esConstrRecursivoDistintoDe e c = esConstrRecursivo e && constructor e /= c

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
