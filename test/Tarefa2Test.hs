module Main where

import Labs2025
import Tarefa2
import Magic

-- ===== Mapas base =====

mComTerraCentro :: Mapa
mComTerraCentro =
  [ [Ar , Ar   , Ar   ]
  , [Ar , Ar   , Ar   ]
  , [Agua, Terra, Pedra]
  ]

mComPedraCentro :: Mapa
mComPedraCentro =
  [ [Ar , Ar   , Ar   ]
  , [Ar , Ar   , Ar   ]
  , [Agua, Pedra, Pedra]
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

-- ===== Minhocas/Estados =====

wBase :: Minhoca
wBase = Minhoca
  { posicaoMinhoca    = Just (1,1)
  , vidaMinhoca       = Viva 100
  , jetpackMinhoca    = 1
  , escavadoraMinhoca = 1
  , bazucaMinhoca     = 1
  , minaMinhoca       = 1
  , dinamiteMinhoca   = 1
  }

mkEstado :: Mapa -> [Objeto] -> [Minhoca] -> Estado
mkEstado m os ws = Estado m os ws

-- Ar com suporte (Terra) por baixo → pode agir/disparar
eArComSuporte :: Estado
eArComSuporte =
  mkEstado mComTerraCentro [] [ wBase { posicaoMinhoca = Just (1,1) } ]

-- Enterrada em Terra (célula atual é Terra) → NÃO dispara
eEnterradaTerra :: Estado
eEnterradaTerra =
  mkEstado mComTerraCentro [] [ wBase { posicaoMinhoca = Just (2,1) } ]

-- Enterrada em Pedra → NÃO dispara
eEnterradaPedra :: Estado
eEnterradaPedra =
  mkEstado mComPedraCentro [] [ wBase { posicaoMinhoca = Just (2,1) } ]

-- Em Ar sem suporte → NÃO dispara
eArSemSuporte :: Estado
eArSemSuporte =
  mkEstado mSemSuporte [] [ wBase { posicaoMinhoca = Just (1,1) } ]

-- Na água → NÃO dispara
eNaAgua :: Estado
eNaAgua =
  mkEstado mTopoAgua [] [ wBase { posicaoMinhoca = Just (0,1) } ]

-- ===== Testes =====
-- (o feedback mostra o resultado de efetuaJogada; aqui apenas definimos os cenários)

testesT2 :: [(NumMinhoca, Jogada, Estado)]
testesT2 =
  [ -- Disparos válidos (Ar + suporte)
    (0, Dispara Bazuca   Este, eArComSuporte)
  , (0, Dispara Mina     Este, eArComSuporte)
  , (0, Dispara Dinamite Este, eArComSuporte)

    -- Disparos inválidos: enterrada / sem suporte / água
  , (0, Dispara Bazuca   Este, eEnterradaTerra)
  , (0, Dispara Bazuca   Este, eEnterradaPedra)
  , (0, Dispara Bazuca   Este, eArSemSuporte)
  , (0, Dispara Bazuca   Este, eNaAgua)

    -- Movimentos (exemplos mínimos)
  , (0, Move Este , eArComSuporte)   -- deve mover
  , (0, Move Norte, eArSemSuporte)   -- não deve mover
  ]

-- ===== Feedback runner =====

dataTarefa2 :: IO TaskData
dataTarefa2 = do
  let ins = testesT2
  outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
  return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2