module Main where

import Labs2025
import Tarefa2
import Magic

-- =====================================================
-- MAPAS BASE (3x3) — coordenadas (linha,coluna)
-- =====================================================

mArCentro :: Mapa
mArCentro =
  [ [Ar , Ar   , Ar   ]
  , [Ar , Ar   , Ar   ]
  , [Agua, Ar  , Pedra]
  ]

mComTerraCentro :: Mapa
mComTerraCentro =
  [ [Ar , Ar   , Ar   ]
  , [Ar , Terra, Ar   ]
  , [Agua, Ar  , Pedra]
  ]

mComPedraCentro :: Mapa
mComPedraCentro =
  [ [Ar , Ar   , Ar   ]
  , [Ar , Pedra, Ar   ]
  , [Agua, Ar  , Pedra]
  ]

mSemSuporte :: Mapa
mSemSuporte =
  [ [Ar , Ar , Ar]
  , [Ar , Ar , Ar]
  , [Ar , Ar , Ar]
  ]

mTopoAgua :: Mapa
mTopoAgua =
  [ [Agua, Agua, Agua]
  , [Ar  , Ar  , Ar  ]
  , [Ar  , Ar  , Ar  ]
  ]

-- =====================================================
-- MINHOCAS & ESTADOS AUXILIARES
-- =====================================================

wBase :: Minhoca
wBase =
  Minhoca { posicaoMinhoca   = Just (1,1)
          , vidaMinhoca      = Viva 100
          , jetpackMinhoca   = 1
          , escavadoraMinhoca= 1
          , bazucaMinhoca    = 1
          , minaMinhoca      = 1
          , dinamiteMinhoca  = 1
          }

estado :: Mapa -> [Objeto] -> [Minhoca] -> Estado
estado m os ws = Estado { mapaEstado = m, objetosEstado = os, minhocasEstado = ws }

-- minhoca em Ar, com suporte (Terra) por baixo
eArComSuporte :: Estado
eArComSuporte =
  estado mComTerraCentro [] [ wBase { posicaoMinhoca = Just (1,1) } ]

-- minhoca enterrada (Terra)
eEnterradaTerra :: Estado
eEnterradaTerra =
  estado mComTerraCentro [] [ wBase { posicaoMinhoca = Just (1,1) } ]

-- minhoca enterrada (Pedra)
eEnterradaPedra :: Estado
eEnterradaPedra =
  estado mComPedraCentro [] [ wBase { posicaoMinhoca = Just (1,1) } ]

-- minhoca em Ar sem suporte
eArSemSuporte :: Estado
eArSemSuporte =
  estado mSemSuporte [] [ wBase { posicaoMinhoca = Just (1,1) } ]

-- minhoca na água (topo)
eNaAgua :: Estado
eNaAgua =
  estado mTopoAgua [] [ wBase { posicaoMinhoca = Just (0,1) } ]

-- minhoca fora do mapa (posição inválida)
eForaMapa :: Estado
eForaMapa =
  estado mArCentro [] [ wBase { posicaoMinhoca = Just (3,3) } ]  -- fora

-- para colisões: um barril à direita
eComBarrilADireita :: Estado
eComBarrilADireita =
  estado mComTerraCentro [Barril (1,2) False] [ wBase { posicaoMinhoca = Just (1,1) } ]

-- =====================================================
-- TESTES — cobrindo as regras do enunciado
-- Cada teste é (idxMinhoca, Jogada, EstadoInicial)
-- O feedback calcula o EstadoEsperado correndo efetuaJogada
-- =====================================================

testesT2 :: [(NumMinhoca, Jogada, Estado)]
testesT2 =
  [ -- MOVIMENTO: inválidos ficam no mesmo sítio
    (0, Move Norte    , eArSemSuporte)      -- no ar, sem suporte → não move
  , (0, Move Este     , eComBarrilADireita) -- célula destino ocupada por objeto → não move
  , (0, Move Este     , eEnterradaTerra)    -- enterrada → não move
  , (0, Move Este     , eEnterradaPedra)    -- enterrada → não move
  , (0, Move Norte    , eForaMapa)          -- fora do mapa → não move

    -- MOVIMENTO: válido (em Ar com suporte e destino livre)
  , (0, Move Este     , eArComSuporte)

    -- DISPARAR: só dispara se em Ar com suporte; nos outros casos não cria objeto
  , (0, Dispara Bazuca   Este, eArComSuporte)   -- válido → cria disparo
  , (0, Dispara Mina     Este, eArComSuporte)   -- válido → cria disparo
  , (0, Dispara Dinamite Este, eArComSuporte)   -- válido → cria disparo

  , (0, Dispara Bazuca   Este, eEnterradaTerra) -- enterrada Terra → não dispara
  , (0, Dispara Bazuca   Este, eEnterradaPedra) -- enterrada Pedra → não dispara
  , (0, Dispara Bazuca   Este, eArSemSuporte)   -- sem suporte → não dispara
  , (0, Dispara Bazuca   Este, eNaAgua)         -- na água → não dispara (se morre, feedback mostra)

  , (0, Dispara Jetpack  Norte, eArComSuporte)  -- não gera objeto; apenas consome munição (se implementado)
  , (0, Dispara Escavadora Este , eArComSuporte) -- idem
  ]

-- =====================================================
-- EXECUÇÃO DO SISTEMA DE FEEDBACK
-- =====================================================

dataTarefa2 :: IO TaskData
dataTarefa2 = do
  let ins = testesT2
  outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
  return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2