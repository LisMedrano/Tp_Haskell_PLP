-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util
import Data.List (zipWith4)

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
-- Tomamos l como inicio del intervalo de la segunda casilla (-inf, l) [l, l+t] ...
-- tamIntervalo calcula el tamaño de cada intervalo
-- La lista tiene n + 2 lugares, n para los del rango y 2 para los que estan fuera
vacio n (l, u) = Histograma l (tamIntervalo l u n) (replicate (n + 2) 0)
  where
    tamIntervalo :: Float -> Float -> Int -> Float
    tamIntervalo l u n = (u - l) / fromIntegral n


-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
-- l y t se mantienen, pero en la lista aumento uno al intervalo al que pertenece x
agregar x his = Histograma l t (actualizarElem (intervaloDe x (l,u) t xs) (1 +) xs)
  where
    l = lower his
    u = upper his
    t = sizeHistograma his
    xs = listaHistograma his

-- Funciones Auxiliares Propias
intervaloDe :: Float -> (Float, Float) -> Float -> [Int] -> Int
intervaloDe x (l,u) t xs
      | x < l = 0 -- si x esta por debajo del interval, primer casillero
      | x > u = length xs - 1 -- si x esta por encima, ultimo casillero
      | otherwise = 1 + floor ((x - l) / t) -- si no, va al cassillero que corresponde

-- aux para encontrar "u", el borde superior
upper :: Histograma -> Float
upper (Histograma l t xs) = l + t * (fromIntegral (length xs) - 2)

-- aux para encontrar "l", el borde inferior
lower :: Histograma -> Float
lower (Histograma l t xs) = l

-- aux que devuelve el tamaño de cada intervalo
sizeHistograma :: Histograma -> Float
sizeHistograma (Histograma l t xs) = t

-- aux que devuelve la lista de numeros del histograma
listaHistograma :: Histograma -> [Int]
listaHistograma (Histograma l t xs) = xs

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
-- histograma n r xs = error "COMPLETAR EJERCICIO 5"
histograma n r xs = Histograma l t (elemsPorCasilero xs)
  where
    l = fst r
    t = (snd r - fst r) / fromIntegral n -- Calculo tamño de los intervaloss
    -- Por empiezo con una lista tamño n+2 de 0, luego por cada elemento usando nuestra funcion
    --intervaloDe, incrementa la cantidad de numeros en ese intervalo
    elemsPorCasilero :: [Float] -> [Int]
    elemsPorCasilero = foldl (\acc x -> actualizarElem (intervaloDe x r t acc) (+1) acc) (replicate (n + 2) 0)

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros (Histograma i t cs) = zipWith4 Casillero limiteInf limiteSup cs p
  where
    cantidadesTotal = map (++) cs
    n = length cs - 1
    limiteInf = infinitoNegativo : [i ,i + t, i+(t*2)... i + (t*n)]
    limiteSup = [i ,i + t, i+(t*2)... i + (t*n)] : infinitoPositivo
    porcentaje = map (\c -> c/cantidadesTotal * 100) cs

