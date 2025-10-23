{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025

livreDeMinhocas :: Posicao -> Estado -> Bool
livreDeMinhocas posi estado = livre posi (minhocasEstado estado)
  where
    livre _ [] = True
    livre posAtual (m:ms)
        | posicaoMinhoca m == Just posAtual = False
        | otherwise = livre posAtual ms

livreDeBarris :: Posicao -> Estado -> Bool
livreDeBarris posi estado = livre posi (objetosEstado estado)
  where
    livre _ [] = True
    livre posAtual (o:os)
        | ehBarril o && posicaoBarril o == posAtual = False
        | otherwise = livre posAtual os

    ehBarril (Barril _ _) = True
    ehBarril _ = False

-- Verifica recursivamente se todas as posições de minhocas estão livres
verificaMinhocas :: [Minhoca] -> Estado -> Bool
verificaMinhocas [] _ = True
verificaMinhocas (m:ms) est =
    case posicaoMinhoca m of
        Just posi  ->
            if livreDeMinhocas posi (est { minhocasEstado = ms })
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





{-
 Apos ler o guiao do projeto percebemos de que as funcoes que se encontram acima
 percebemos que ainda que ao rodar "cabal run t1-feedback" com um estado teste, o nosso codigo aparecia como correto
 o unico problema é que nao respeitava as condicoes impostas pelo corpo docente da UC

 para que tal acontece necessitamos das seguintes funcoes:
-}


-- Verifica se o mapa é retangular e não vazio
validaMapa :: Mapa -> Bool
validaMapa [] = False
validaMapa (h:t) =
    | -- condicao para que um mapa seja valide = True
    | otherwise = False

-- Verifica se uma posição está dentro dos limites do mapa
dentroMapa :: Posicao -> Mapa -> Bool
dentro (x,y) [] = False

-- Obtém o terreno existente numa posição (se for válida)
terrenoEm :: Posicao -> Mapa -> Maybe Terreno

-- Determina se o terreno é opaco (não atravessável) 
eTerrenoOpaco :: Terreno -> Bool --(retirada do ficheiro: Tarefa0_2025.hs)
eTerrenoOpaco terreno =
    case terreno of
        Terra -> True
        Pedra -> True
        _ -> False
