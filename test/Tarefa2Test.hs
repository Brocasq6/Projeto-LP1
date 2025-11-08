{-# OPTIONS_GHC -Wno-unused-binds #-}
module Main where

import Labs2025
import Tarefa2
import Magic

-- =====================================================
-- MAPAS BASE
-- =====================================================

mapaSimples :: Mapa
mapaSimples =
  [ [Ar, Ar, Ar]
  , [Ar, Terra, Ar]
  , [Agua, Ar, Pedra]
  ]

mapaComPedra :: Mapa
mapaComPedra =
  [ [Ar, Ar, Ar]
  , [Ar, Pedra, Ar]
  , [Agua, Ar, Pedra]
  ]

mapaComBarril :: Mapa
mapaComBarril =
  [ [Ar, Ar, Ar, Ar]
  , [Ar, Terra, Ar, Ar]
  , [Agua, Ar, Pedra, Ar]
  ]

mapaTopoAgua :: Mapa
mapaTopoAgua =
  [ [Agua, Agua, Agua]
  , [Ar, Terra, Ar]
  , [Agua, Ar, Pedra]
  ]

-- =====================================================
-- MINHOCAS AUXILIARES
-- =====================================================

minhocaBase :: Minhoca
minhocaBase = Minhoca
  { posicaoMinhoca = Just (1,1)
  , vidaMinhoca = Viva 100
  , jetpackMinhoca = 1
  , escavadoraMinhoca = 1
  , bazucaMinhoca = 1
  , minaMinhoca = 1
  , dinamiteMinhoca = 1
  }

minhocaSemMunicao :: Minhoca
minhocaSemMunicao = Minhoca
  { posicaoMinhoca = Just (1,1)
  , vidaMinhoca = Viva 100
  , jetpackMinhoca = 0
  , escavadoraMinhoca = 0
  , bazucaMinhoca = 0
  , minaMinhoca = 0
  , dinamiteMinhoca = 0
  }

minhocaExtra :: Posicao -> Minhoca
minhocaExtra p = minhocaBase { posicaoMinhoca = Just p }

-- =====================================================
-- ESTADOS BASE
-- =====================================================

eBase :: Estado
eBase =
  Estado
    { mapaEstado = mapaSimples
    , objetosEstado = []
    , minhocasEstado = [minhocaBase]
    }

-- minhoca está no ar (linha 0)
eNoAr :: Estado
eNoAr =
  eBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Just (0,1) }] }

-- minhoca dentro de água
eEmAgua :: Estado
eEmAgua =
  eBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Just (2,0) }] }

-- minhoca junto de pedra
eContraPedra :: Estado
eContraPedra =
  Estado
    { mapaEstado = mapaComPedra
    , objetosEstado = []
    , minhocasEstado = [minhocaBase]
    }

-- minhoca junto de terra
eContraTerra :: Estado
eContraTerra =
  eBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Just (1,0) }] }

-- minhoca pode saltar (posição de chão)
eSaltoValido :: Estado
eSaltoValido = eBase

-- duas minhocas próximas
eComOutraMinhoca :: Estado
eComOutraMinhoca =
  eBase { minhocasEstado = [minhocaBase, minhocaExtra (1,2)] }

-- minhoca sem munição
eSemMunicao :: Estado
eSemMunicao =
  eBase { minhocasEstado = [minhocaSemMunicao] }

-- já existe disparo do mesmo tipo (bazuca)
eComObjetoMesmoTipo :: Estado
eComObjetoMesmoTipo =
  Estado
    { mapaEstado = mapaSimples
    , objetosEstado = [Disparo (1,2) Este Bazuca Nothing 0]
    , minhocasEstado = [minhocaBase]
    }

-- minhoca fora do mapa (fora dos limites)
eForaMapa :: Estado
eForaMapa =
  eBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Just (3,3) }] }

-- duas minhocas no mapa
eDuasMinhocas :: Estado
eDuasMinhocas =
  eBase { minhocasEstado = [minhocaBase, minhocaExtra (0,0)] }

-- há um barril no mapa
eComBarril :: Estado
eComBarril =
  Estado
    { mapaEstado = mapaComBarril
    , objetosEstado = [Barril (1,2) False]
    , minhocasEstado = [minhocaBase]
    }

-- minhoca no topo do mapa
eTopoMapa :: Estado
eTopoMapa =
  eBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Just (0,1) }] }

-- minhoca no topo mas sobre água
eTopoMapaAgua :: Estado
eTopoMapaAgua =
  eBase { mapaEstado = mapaTopoAgua
        , minhocasEstado = [minhocaBase { posicaoMinhoca = Just (0,1) }]
        }

-- =====================================================
-- TESTES T2 (50 jogadas cobrindo todas as regras)
-- =====================================================

testesT2 :: [(NumMinhoca, Jogada, Estado)]
testesT2 =
  [ (0, Move Este, eBase)
  , (0, Move Oeste, eBase)
  , (0, Move Norte, eBase)
  , (0, Move Nordeste, eBase)
  , (0, Move Noroeste, eBase)
  , (0, Move Sul, eBase)
  , (0, Move Norte, eEmAgua)
  , (0, Move Norte, eContraPedra)
  , (0, Move Norte, eContraTerra)
  , (0, Move Nordeste, eSaltoValido)
  , (0, Move Noroeste, eSaltoValido)
  , (0, Move Oeste, eComOutraMinhoca)
  , (0, Dispara Bazuca Este, eBase)
  , (0, Dispara Mina Este, eBase)
  , (0, Dispara Dinamite Este, eBase)
  , (0, Dispara Jetpack Este, eBase)
  , (0, Dispara Escavadora Este, eBase)
  , (0, Dispara Bazuca Este, eSemMunicao)
  , (0, Dispara Mina Este, eSemMunicao)
  , (0, Dispara Dinamite Este, eSemMunicao)
  , (0, Dispara Jetpack Este, eSemMunicao)
  , (0, Dispara Escavadora Este, eSemMunicao)
  , (0, Dispara Bazuca Este, eComObjetoMesmoTipo)
  , (0, Dispara Mina Este, eComObjetoMesmoTipo)
  , (0, Dispara Dinamite Este, eComObjetoMesmoTipo)
  , (0, Move Norte, eForaMapa)
  , (1, Move Este, eDuasMinhocas)
  , (1, Dispara Mina Sul, eDuasMinhocas)
  , (1, Dispara Bazuca Oeste, eDuasMinhocas)
  , (1, Dispara Dinamite Oeste, eDuasMinhocas)
  , (1, Move Noroeste, eDuasMinhocas)
  , (1, Move Nordeste, eDuasMinhocas)
  , (0, Dispara Dinamite Norte, eBase)
  , (0, Dispara Mina Norte, eBase)
  , (0, Move Oeste, eComBarril)
  , (0, Move Norte, eComBarril)
  , (0, Move Nordeste, eComBarril)
  , (0, Dispara Bazuca Norte, eComBarril)
  , (0, Dispara Dinamite Norte, eComBarril)
  , (0, Move Sul, eTopoMapa)
  , (0, Move Norte, eTopoMapa)
  , (0, Move Norte, eTopoMapaAgua)
  , (0, Dispara Escavadora Sul, eBase)
  , (0, Dispara Escavadora Norte, eBase)
  , (0, Dispara Jetpack Norte, eBase)
  , (0, Dispara Jetpack Oeste, eBase)
  , (0, Dispara Jetpack Nordeste, eBase)
  , (0, Dispara Jetpack Noroeste, eBase)
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
