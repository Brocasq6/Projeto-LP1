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

-- Verifica recursivamente se todas as posições de minhocas estão livres
verificaMinhocas :: [Minhoca] -> Estado -> Bool
verificaMinhocas [] _ = True
verificaMinhocas (m:ms) est =
    case posicaoMinhoca m of
        Just p  -> if livreDeMinhocas p est then verificaMinhocas ms est else False
        Nothing -> verificaMinhocas ms est  -- minhoca sem posição é considerada válida

-- Verifica recursivamente se todas as posições de barris estão livres
verificaBarris :: [Objeto] -> Estado -> Bool
verificaBarris [] _ = True
verificaBarris (b:bs) est =
    | livreDeBarris (posicaoBarril b) e = verificaBarris bs est
    | otherwise = False 


    