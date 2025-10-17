module Main where

import Labs2025
import Tarefa1
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = [estadoExemplo2]

estadoExemplo2 :: Estado
Estado 
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Ar,Ar]
        ,[Ar,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra]
        ,[Pedra,Pedra,Pedra,Pedra,Pedra,Agua,Agua,Agua,Agua,Agua,Agua,Agua,Agua,Agua,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]
        ,[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Agua,Agua,Agua,Agua,Agua,Agua,Agua,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]
        ,[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Agua,Agua,Agua,Agua,Agua,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]
        ,[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]
        ,[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (2,2), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,0), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (3,15), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1
