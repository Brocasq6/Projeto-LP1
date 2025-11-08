module Main where

import Labs2025
import Tarefa2
import Magic ()  -- se não tiveres Magic, remove esta linha

-- ─────────────────────────────
-- Mapas simples de apoio
-- ─────────────────────────────
mapaBase :: Mapa
mapaBase =
  [ [Ar,   Ar,   Ar  ]
  , [Ar,   Terra,Ar  ]
  , [Agua, Ar,   Pedra]
  ]

mapaChao :: Mapa
mapaChao =
  [ [Ar, Ar, Ar]
  , [Ar, Ar, Ar]
  , [Terra,Terra,Terra]
  ]

mapaComTerraAoLado :: Mapa
mapaComTerraAoLado =
  [ [Ar, Ar, Ar]
  , [Terra,Ar, Terra]
  , [Ar, Ar, Ar]
  ]

-- ─────────────────────────────
-- Minhocas “base”
-- ─────────────────────────────
wFull :: Minhoca
wFull = Minhoca (Just (1,1)) (Viva 100) 3 3 3 3 3

wNoAmmo :: Minhoca
wNoAmmo = Minhoca (Just (1,1)) (Viva 100) 0 0 0 0 0

wAir :: Minhoca
wAir = wFull { posicaoMinhoca = Just (0,1) }

wWater :: Minhoca
wWater = wFull { posicaoMinhoca = Just (2,0) }

wEdge :: Minhoca
wEdge = wFull { posicaoMinhoca = Just (0,0) }

wTopCenter :: Minhoca
wTopCenter = wFull { posicaoMinhoca = Just (0,1) }

wLeft :: Minhoca
wLeft = wFull { posicaoMinhoca = Just (1,0) }

wRight :: Minhoca
wRight = wFull { posicaoMinhoca = Just (1,2) }

wDead :: Minhoca
wDead = wFull { vidaMinhoca = Morta }

-- ─────────────────────────────
-- Estados “one-worm”
-- ─────────────────────────────
eBase      = Estado mapaBase [] [wFull]
eAr        = Estado mapaBase [] [wAir]
eAgua      = Estado mapaBase [] [wWater]
eBorda     = Estado mapaBase [] [wEdge]
eTopo      = Estado mapaBase [] [wTopCenter]
eEsq       = Estado mapaBase [] [wLeft]
eDir       = Estado mapaBase [] [wRight]
eChao      = Estado mapaChao [] [wFull]
eSemMun    = Estado mapaBase [] [wNoAmmo]
eMorta     = Estado mapaBase [] [wDead]
eTerraLado = Estado mapaComTerraAoLado [] [wFull]

-- Já existe um disparo igual (Bazuca do dono 0)
eComBazucaIgual = Estado mapaBase [Disparo (1,2) Este Bazuca Nothing 0] [wFull]

-- Posição de destino ocupada por barril
eBarrilADireita = Estado mapaBase [Barril (1,2) False] [wFull]

-- Posição de destino ocupada por minhoca
eOutraMinhocaADireita =
  Estado mapaBase [] [ wFull, wFull { posicaoMinhoca = Just (1,2) } ]

-- Para testar Mina/Dinamite quando destino não livre
eDestinoBloqueadoPorPedraTopo =
  Estado
    [ [Pedra,Pedra,Pedra]
    , [Ar,   Ar,   Ar   ]
    , [Ar,   Ar,   Ar   ]
    ]
    []
    [wTopCenter]

-- ─────────────────────────────
-- TESTES – cada um cobre uma regra do enunciado
-- ─────────────────────────────
testesT2 :: [(NumMinhoca, Jogada, Estado)]
testesT2 =
  [
  -- MOVIMENTO
    -- 1) Qualquer movimentação inválida → fica na mesma
    (0, Move Oeste, eEsq)                           -- destino fora do mapa
  , (0, Move Este,  eBarrilADireita)                -- destino ocupado por objeto
  , (0, Move Este,  eOutraMinhocaADireita)          -- destino ocupado por minhoca

    -- 2) Só se move se estiver viva
  , (0, Move Este,  eMorta)

    -- 3) Chão tem de estar livre
  , (0, Move Norte, eTerraLado)                     -- acima é Terra à esquerda/direita

    -- 4) Mover para cima só se estiver no chão (salto)
  , (0, Move Norte, eAr)                            -- no ar, não pode
  , (0, Move Nordeste, eAr)
  , (0, Move Noroeste, eAr)

    -- 5) No ar não se move
  , (0, Move Este,  eAr)

    -- 6) Se se move para fora do mapa → morre e fica sem posição
  , (0, Move Norte, eBorda)

    -- 7) Se se move para água → morre e fica na nova posição
  , (0, Move Sul,   Estado mapaBase [] [wTopCenter])  -- (1,1) é Terra; força test depois com outro mapa
  , (0, Move Sul,   Estado
                      [ [Ar, Ar, Ar]
                      , [Agua,Ar, Ar]
                      , [Ar, Ar, Ar]
                      ]
                      []
                      [wEdge { posicaoMinhoca = Just (0,0) }])

  -- DISPARO
    -- 8) Para disparar tem de estar viva e ter munição (>0); consome 1
  , (0, Dispara Bazuca Este,    eBase)
  , (0, Dispara Mina Este,      eBase)
  , (0, Dispara Dinamite Este,  eBase)
  , (0, Dispara Jetpack Este,   eBase)
  , (0, Dispara Escavadora Este,eBase)

    -- 9) Sem munição → não dispara
  , (0, Dispara Bazuca Este,   eSemMun)
  , (0, Dispara Mina Este,     eSemMun)
  , (0, Dispara Dinamite Este, eSemMun)
  , (0, Dispara Jetpack Este,  eSemMun)
  , (0, Dispara Escavadora Este,eSemMun)

    -- 10) Não pode existir já um disparo do mesmo tipo para a mesma minhoca
  , (0, Dispara Bazuca Este,   eComBazucaIgual)

    -- 11) Jetpack: permite mover em qualquer direção se destino livre
  , (0, Dispara Jetpack Norte, eChao)
  , (0, Dispara Jetpack Nordeste, eChao)
  , (0, Dispara Jetpack Oeste, eChao)

    -- 12) Escavadora: destrói Terra no destino e move a minhoca para lá
  , (0, Dispara Escavadora Oeste, eTerraLado)       -- há Terra em (1,0)

    -- 13) Bazuca: colocado na posição de destino (ou na atual se bloqueado)
  , (0, Dispara Bazuca Este, eBase)                 -- destino livre
  , (0, Dispara Bazuca Norte, eDestinoBloqueadoPorPedraTopo) -- destino bloqueado → fica na atual

    -- 14) Mina: se destino livre vai para lá, se não livre fica na posição atual
  , (0, Dispara Mina Este,   eBase)
  , (0, Dispara Mina Norte,  eDestinoBloqueadoPorPedraTopo)

    -- 15) Dinamite: se destino livre vai para lá, senão fica na posição atual, tempo=4
  , (0, Dispara Dinamite Este,  eBase)
  , (0, Dispara Dinamite Norte, eDestinoBloqueadoPorPedraTopo)

    -- 16) Objetos colocados fora do mapa são eliminados (destino fora)
  , (0, Dispara Bazuca Oeste,  eEsq)                -- tentar colocar a Oeste do (1,0)
  ]

-- ─────────────────────────────
-- Runner de feedback
-- ─────────────────────────────
dataTarefa2 :: IO TaskData
dataTarefa2 = do
  let ins = testesT2
  outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
  pure $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2