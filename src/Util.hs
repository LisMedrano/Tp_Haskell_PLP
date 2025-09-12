module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha n s = foldr (:) s listaEspaciosNecesarios
  where
    listaEspaciosNecesarios = replicate cantidadEspacios ' '
    cantidadEspacios = max 0 (n - length s)
-- creamos una lista con la cant de ' ' necesarios
-- vamos haciendo ' ' : (' ' : (... : s)) con foldr

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = zipWith (\a i -> if i == n then f a else a) xs [0..]
-- creamos la lista [0,1,2, ...] que representa los indices
-- con zipWith aplicamos una funcion que nos dice si es o no el indice que buscamos.
-- si lo es, aplicamos la funcion f a ese elemento

-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
