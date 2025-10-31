module Main where

import Labs2025
import Tarefa2
import Magic


mapaSimples :: Mapa
mapaSimples =
  [ [Ar, Ar, Ar]
  , [Ar, Terra, Ar]
  , [Agua, Ar, Pedra]
  ]

-- Minhoca viva no centro do mapa
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

-- Estado inicial com 1 minhoca e sem objetos
estadoBase :: Estado
estadoBase = Estado
  { mapaEstado = mapaSimples
  , objetosEstado = []
  , minhocasEstado = [minhocaBase]
  }


-- | Definir aqui os testes do grupo para a Tarefa 2
testesT2 :: [(NumMinhoca, Jogada, Estado)]
testesT2 =
  [ (0, Move Este, eBase)              -- movimento simples
  , (0, Move Oeste, eBase)
  , (0, Move Norte, eBase)
  , (0, Move Nordeste, eBase)
  , (0, Move Noroeste, eBase)
  , (0, Move Sul, eBase)              -- inválido (fora do mapa)
  , (0, Move Norte, eNoAr)            -- inválido (no ar)
  , (0, Move Norte, eEmAgua)          -- morre
  , (0, Move Norte, eContraPedra)     -- bloqueado
  , (0, Move Norte, eContraTerra)
  , (0, Move Nordeste, eSaltoValido)
  , (0, Move Noroeste, eSaltoValido)
  , (0, Move Oeste, eComOutraMinhoca)
  , (0, Dispara Bazuca Este, eBase)   -- cria disparo
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


dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
