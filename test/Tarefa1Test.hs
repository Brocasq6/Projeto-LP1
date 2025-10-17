module Main where

import Labs2025
import Tarefa1
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = [estadoExemplo2]

estadoExemplo2 :: Estado
estadoExemplo2 =
  Estado
    { mapaEstado =
        replicate 10 (replicate 20 Ar)
    , objetosEstado =
        [Barril (2,2) False]  -- barril na mesma posição da segunda minhoca
    , minhocasEstado =
        [ Minhoca
            { posicaoMinhoca = Just (0,0)
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 2
            , escavadoraMinhoca = 1
            , bazucaMinhoca = 3
            , minaMinhoca = 1
            , dinamiteMinhoca = 0
            }
        , Minhoca
            { posicaoMinhoca = Just (2,2)
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 1
            , escavadoraMinhoca = 1
            , bazucaMinhoca = 1
            , minaMinhoca = 1
            , dinamiteMinhoca = 1
            }
        ]
    }

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1
