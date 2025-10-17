{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025

-- Verifica se uma posição está livre de minhocas
livreDeMinhocas :: Posicao -> Estado -> Bool
livreDeMinhocas posi estado = livre posi (minhocasEstado estado)
  where
    livre _ [] = True
    livre posi (m:ms)
        | posicaoMinhoca m == Just posi = False
        | otherwise = livre posi ms

-- Verifica se uma posição está livre de barris
livreDeBarris :: Posicao -> Estado -> Bool
livreDeBarris pos estado = livre pos (objetosEstado estado)
  where
    livre _ [] = True
    livre pos (o:os)
        | ehBarril o && posicaoBarril o == pos = False
        | otherwise = livre pos os

    ehBarril (Barril _ _) = True
    ehBarril _ = False

-- Verifica recursivamente se todas as posições de minhocas estão livres
verificaMinhocas :: [Minhoca] -> Estado -> Bool
verificaMinhocas [] _ = True
verificaMinhocas (m:ms) est =
    case posicaoMinhoca m of
        Just p  ->
            if livreDeMinhocas p (est { minhocasEstado = ms })
            then verificaMinhocas ms est
            else False
        Nothing -> verificaMinhocas ms est

-- Verifica recursivamente se todas as posições de barris estão livres
verificaBarris :: [Objeto] -> Estado -> Bool
verificaBarris [] _ = True
verificaBarris (b:bs) e
    | livreDeBarris (posicaoBarril b) (e { objetosEstado = bs }) = verificaBarris bs e
    | otherwise = False

-- Verifica se o estado é válido
validaEstado :: Estado -> Bool
validaEstado e
    | null (mapaEstado e) || null (head (mapaEstado e)) = False
    | not (verificaMinhocas (minhocasEstado e) e) = False
    | not (verificaBarris [b | b@Barril{} <- objetosEstado e] e) = False
    | otherwise = True