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
testesTarefa2 :: [(NumMinhoca, Jogada, Estado)]
testesTarefa2 =
  [ -- 1️⃣ Movimento simples para a direita (Ar → pode mover)
    (0, Move Este, estadoBase)

  , -- 2️⃣ Movimento para cima (Ar → pode mover)
    (0, Move Norte, estadoBase)

  , -- 3️⃣ Movimento para a esquerda (Ar → pode mover)
    (0, Move Oeste, estadoBase)

  , -- 4️⃣ Movimento para baixo (Agua → morre)
    (0, Move Sul, estadoBase)

  , -- 5️⃣ Movimento bloqueado (Pedra à direita)
    (0, Move Este, estadoBase { mapaEstado =
        [ [Ar, Ar, Pedra]
        , [Ar, Ar, Pedra]
        , [Ar, Ar, Pedra]
        ] })

  , -- 6️⃣ Disparo com Bazuca (cria um novo Disparo)
    (0, Dispara Bazuca Este, estadoBase)
  ]

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
