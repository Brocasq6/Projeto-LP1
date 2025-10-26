{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025







--------------- funcoes que verificam se uma posicao se encontram livres de minhocas ou barris -----------------------------------

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

-----------------------------------------------------------------------------------












-------------------- funcoes que verificam se todas as posicoes se encontram livres de minhocas ou barris -----------------------------------------


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



---------------------------------------------------------------------------------------------------------















----------------------- funcoes de validacao -----------------------


-- valida Minhoca
validaMinhoca :: Minhoca -> Estado -> Bool
validaMinhoca m e = 
    case posicaoMinhoca m of 
        Nothing -> vidaMorta m
        Just p -> dentroMapa p (mapaEstado e)
                  && not (maybe False eTerrenoOpaco (terrenoNaPosicao p (mapaEstado e)))
                  && livreDeBarris posicao estado
                  && livreDeMinhocas posicao estado
                  && vidaValida minhoca
                  && municoesValidas minhoca
                  && | terrenoNaPosicao p (mapaEstado e) == Just Agua = vidaMorta minhoca
                     | otherwise = True

-- valida Objeto
validaObjeto :: Objeto -> Estado -> Bool
validaObjeto o e = 
    case o of
        Barril p _ ->  -- caso em que é um barril
            dentroMapa p (mapaEstado e)
            && not (maybe False eTerrenoOpaco (terrenoNaPosicao p (mapaEstado e)))
            && livreDeMinhocas p e
            && livreDeBarris p (e {objetosEstado = filter (/= o) (objetosEstado e)})
        
        Disparo p _ arma tempo dono ->  -- caso em que é um disparo
            dentroMapa p (mapaEstado e) &&
            tipoDisparoValido arma tempo dono e

        _ -> True -- casos que nao sejam nem barril nem disparo

-- Disparo Valido
tipoDisparoValido :: TipoArma -> Maybe Int -> NumMinhoca -> Estado -> Bool
tipoDisparoValido arma tempo dono e = undefined
    dono >= 0 && dono < length (minhocasEstado e) &&
    case arma of 
        Jetpack -> False
        Escavadora -> False
        Bazuca -> tempo == Nothing
        Mina -> tempo == Nothing || maybe True (`elem` [0..2]) tempo
        Dinamite -> tempo == maybe True (`elem` [0..4]) tempo

-- Verifica se o estado é válido
validaEstado :: Estado -> Bool
validaEstado e = undefined

----------------------------------------------













-- funcoes auxiliares à funcao validaMinhoca ----------

vidaMorta :: Minhoca -> Bool
vidaMorta minhoca =
    case vidaMinhoca minhoca of
        Morta -> True
        _ -> False

vidaValida :: Minhoca -> Bool
vidaValida minhoca =
    case vidaMinhoca minhoca of
        Viva n -> n >= 0 && n <= 100
        Morta -> True

municoesValidas :: Minhoca -> Bool
municoesValidas minh =
    all (>= 0)
    [ jetpackMinhoca minhoca
     , escavadoraMinhoca minhoca
     , bazucaMinhoca minhoca
     , minaMinhoca minhoca
     , dinamiteMinhoca minhoca
    ]

----------------------------------------------






















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
dentroMapa (x,y) mapa =
    x >= 0 && x < length (head mapa) && y >= 0 && y < length mapa

-- Obtém o terreno existente numa posição (se for válida)
terrenoNaPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoNaPosicao (x,y) [] = Nothing
terrenoNaPosicao (x,y) mapa 
    | dentroMapa (x,y) mapa = Just ((mapa!!y)!!x) 
    | otherwise = Nothing

-- Determina se o terreno é opaco (não atravessável) 
eTerrenoOpaco :: Terreno -> Bool --(retirada do ficheiro: Tarefa0_2025.hs)
eTerrenoOpaco terreno =
    case terreno of
        Terra -> True
        Pedra -> True
        _ -> False
