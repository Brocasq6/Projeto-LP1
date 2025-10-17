{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025

-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado e
    | null (mapaEstado e) = False  -- mapa vazio
    | not (verificaMinhocas (minhocasEstado e) e) = False
    | not (verificaBarris [b | b@Barril{} <- objetosEstado e] e) = False --filtra apenas os objetos do tipo Barril
    | otherwise = True

-- Verifica se uma posição está livre de minhocas
livreDeMinhocas :: Posicao -> Estado -> Bool
livreDeMinhocas pos est
    | minhocasOcupam pos (minhocasEstado est) = False
    | otherwise = True
  where
    minhocasOcupam :: Posicao -> [Minhoca] -> Bool
    minhocasOcupam _ [] = False
    minhocasOcupam p (m:ms) =
        case posicaoMinhoca m of
            Just posM -> posM == p || minhocasOcupam p ms
            Nothing   -> minhocasOcupam p ms


-- Verifica se uma posição está livre de barris
livreDeBarris :: Posicao -> Estado -> Bool
livreDeBarris pos est
    | barrisOcupam pos (objetosEstado est) = False
    | otherwise = True
  where
    barrisOcupam :: Posicao -> [Objeto] -> Bool
    barrisOcupam _ [] = False
    barrisOcupam p (b:bs) =
        case b of
            Barril { posicaoBarril = pb } -> pb == p || barrisOcupam p bs
            _ -> barrisOcupam p bs