module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test 
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
      
    ]

-- Ej 1
testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 0 "noAgregaEspacios" ~?= "noAgregaEspacios",
      alinearDerecha (-1) "negativo" ~?= "negativo",
      alinearDerecha 14 "espaciosJustos" ~?= "espaciosJustos",
      alinearDerecha 1 "" ~?= " ",
      alinearDerecha 0 "" ~?= "",
      alinearDerecha 1 " " ~?= " ",
      alinearDerecha 12 " conEspacio" ~?= "  conEspacio"
    ]

-- Ej 2
testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 2 (+ 10) [1, 2, 3] ~?= [1, 2, 13],
      actualizarElem 3 (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem (-1) (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 0 (+ 1) [1, 2, 3] ~?= [2, 2, 3],
      actualizarElem 0 (+ 1) [] ~?= []
    ]

-- Ej 3
testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ], 
      casilleros (vacio 2 (-5, -1)) -- caso rangos negativos
        ~?= [ Casillero infinitoNegativo (-5) 0 0,
              Casillero (-5) (-3) 0 0,
              Casillero (-3) (-1) 0 0,
              Casillero (-1) infinitoPositivo 0 0
            ],
        casilleros (vacio 2 (1.5, 5.5)) -- caso con rangos con coma
        ~?= [ Casillero infinitoNegativo 1.5 0 0,
              Casillero 1.5 3.5 0 0,
              Casillero 3.5 5.5 0 0,
              Casillero 5.5 infinitoPositivo 0 0
            ] 
    ]

-- Ej 4
testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6) 
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 6 h0) --Caso extremo [4 , +infinito)
           ~?= [  Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100 -- El 100% de los valores están acá
                ],
          casilleros (agregar 1 (agregar 2 h0)) --Caso aplicado 2 veces la funcion 
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 50, --El 50% de los valores están acá
                  Casillero 2 4 1 50, --El 50% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0 
                ]
         
        ]

-- Ej 5
testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
     
      histograma 2 (1.5, 3) [1.5, 3.75, 2.225] ~?= agregar 2.225 (agregar 3.75 (agregar 1.5 (vacio 2 (1.5, 3)))), --Caso con numeros con coma en rangos y lista

      histograma 3 (-6, -1) [-8, -4, 1] ~?= agregar 1 (agregar (-4) (agregar (-8) (vacio 3 (-6, -1)))), --Caso negativos en rango y lista

      histograma 3 (1, 3) [] ~?= vacio 3 (1, 3) -- Caso lista vacia
    ]

-- Ej 6
testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (histograma 2 (1,5) [3]) --Caso con la funcion histograma
        ~?= [ Casillero infinitoNegativo 1.0 0 0.0,
              Casillero 1.0 3.0 0 0.0,
              Casillero 3.0 5.0 1 100.0,
              Casillero 5.0 infinitoPositivo 0 0.0
            ],
       casilleros (histograma 2 (1,5) [3, 2, -1, -2]) --Caso con dif porcentajes
        ~?= [ Casillero infinitoNegativo 1.0 2 50.0,
              Casillero 1.0 3.0 1 25.0,
              Casillero 3.0 5.0 1 25.0,
              Casillero 5.0 infinitoPositivo 0 0.0
            ]      
    ]

-- Ej 7
testsRecr :: Test
testsRecr =
  test
    [ -- True: en la raíz los dos hijos son iguales
      hayNodoConSubarbolesIguales (Suma (Const 1) (Const 1)) ~?= True,

      -- False: en la raíz (2) != (3) 
      hayNodoConSubarbolesIguales (Mult (Const 2) (Const 3)) ~?= False,

      -- True: más profundo; los dos SUMA internos son iguales
      hayNodoConSubarbolesIguales
        (Suma (Suma (Const 1) (Const 2)) (Suma (Const 1) (Const 2)))
        ~?= True
    ]
  where
    hayNodoConSubarbolesIguales :: Expr -> Bool
    hayNodoConSubarbolesIguales = recrExpr (const False) (\_ _ -> False) operacion operacion operacion operacion
                                  where
                                    operacion reci recd i d =  reci || recd || (i==d) 

-- Ej 7
testsFold :: Test
testsFold =
  test
    [ tamFold (Const 5) ~?= 1,
      tamFold (Suma (Resta (Const 1) (Const 2)) (Mult (Const 3) (Const 4))) ~?= 7,
      tamFold (Div (Suma (Const 1) (Const 2)) (Rango 0 1)) ~?= 5
    ]
  where
    tamFold :: Expr -> Int 
    tamFold = foldExpr (const 1) (\_ _ -> 1) operacion operacion operacion operacion
                                  where
                                    operacion reci recd = 1 + reci + recd

-- Ej 8
testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      -- 
      fst (eval (Div (Const 8) (Const 4)) genFijo) ~?= 2.0,
      
      fst (eval (Suma (Const 1) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 3.7980492,

      -- Probamos que un rango de x a x es lo mismo que evaluar directamente x
      fst (eval (Const 2) (genNormalConSemilla 4)) ~?= 
      fst (eval (Rango 2 2) (genNormalConSemilla 4))
    ]

-- Ej 9
testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [ casilleros (fst (armarHistograma 3 3 (dameUno (1,5)) (genNormalConSemilla 5))) ~?=[Casillero infinitoNegativo 1.9035962 0 0.0,
       Casillero 1.9035962 2.8586945999999998 1 (1/3 * 100),
       Casillero 2.8586945999999998 3.813793 1 (1/3 * 100), 
       Casillero 3.813793 4.7688914 1 (1/3 * 100),
       Casillero 4.7688914 infinitoPositivo 0 0.0],
      
      -- Probamos que con muchos valores si llega a cubrir los valores fuera de rango
      -- a diferencia de con pocos valores que queda con 0 elementos
      casilleros (fst (armarHistograma 3 10000 (dameUno (1,5)) (genNormalConSemilla 7))) ~?=[Casillero infinitoNegativo 1.0287318 263 (263/10000 * 100),
       Casillero 1.0287318 2.3566919000000004 2304 (2304/10000 * 100),
       Casillero 2.3566919000000004 3.6846520000000003 4852 (4852/10000 * 100), 
       Casillero 3.6846520000000003 5.01261213 2338 (2338/10000 * 100),
       Casillero 5.01261213 infinitoPositivo 243 (243/10000 * 100)],

      -- Probamos que si ponemos siempre el mismo valor, caen todos en el mismo casillero
      casilleros (fst (armarHistograma 4 3 (\x -> (2,x)) (genNormalConSemilla 7))) ~?=[Casillero infinitoNegativo 1.0 0 0.0,
       Casillero 1.0 1.5 0 0.0,
       Casillero 1.5 2.0 0 0.0, 
       Casillero 2.0 2.5 3 100.0,
       Casillero 2.5 3.0 0 0.0,
       Casillero 3.0 infinitoPositivo 0 0.0]
    ]

-- Ej 10
testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [-- Probamos que se distribuyen os valores correctamente en diferentes casilleros
    -- si la expresion contiene un rango
    casilleros (fst (evalHistograma 3 4 (Rango 1 3) (genNormalConSemilla 4))) ~?= 
    [Casillero infinitoNegativo 1.2331247 0 0.0,
     Casillero 1.2331247 1.57619244 1 (1/4 * 100),
     Casillero 1.57619244 1.9192603 2 (2/4 * 100),
     Casillero 1.9192603 2.26232792 1 (1/4 * 100),
     Casillero 2.26232792 infinitoPositivo 0 0.0],
     
    -- Probamos que si la expresion no tiene un rango (es constante) los valores caen siempre en el mismo casillero
    casilleros (fst (evalHistograma 3 4 (Const 2) (genNormalConSemilla 4))) ~?= 
    [Casillero infinitoNegativo 1.0 0 0.0,
     Casillero 1.0 1.6666667 0 0.0,
     Casillero 1.6666667 2.3333334 4 100.0,
     Casillero 2.3333334 3.0000001 0 0.0,
     Casillero 3.0000001 infinitoPositivo 0 0.0]

    -- Aclaracion: No hacemos tests con expresiones mas complejas como sumas, restas, etc
    -- porque nos interesa ver como caen los valores finales en el histograma
    -- y no que se calculen bien, ya que eso es parte de los tests de la funcion eval
    -- que evalua expresiones
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

-- Ej 11
testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0",
      
      -- Tests propios
      mostrar (parse "1") ~?= "1.0",
      mostrar (parse "1+2") ~?= "1.0 + 2.0",
      mostrar (parse "1*(1~2)") ~?= "1.0 * 1.0~2.0",
      mostrar (parse "-1 / (3-1)") ~?= "-1.0 / (3.0 - 1.0)"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
