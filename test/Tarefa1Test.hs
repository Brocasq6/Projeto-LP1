module Main where

import Labs2025
import Tarefa1
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = [estadoExemplo1]

estadoExemplo1 :: Estado
estadoExemplo1 =
  Estado
    { mapaEstado = replicate 3 (replicate 3 Terra)
    , objetosEstado = [Barril (2,2) False]
    , minhocasEstado = [Minhoca (Just (1,1)) (Viva 100) 1 1 1 1 1]
    }

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1
