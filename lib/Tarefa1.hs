{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}

module Tarefa1 where

import Labs2025







--------------- funcoes que verificam se uma posicao se encontram livres de minhocas ou barris -----------------------------------
-- | Verifica se uma posição está livre de minhocas num dado estado.
livreDeMinhocas :: Posicao -> Estado -> Bool
livreDeMinhocas posicao estado = livre posicao (minhocasEstado estado)
  where
    livre _ [] = True
    livre posAtual (minhoca:minhocas)
        | posicaoMinhoca minhoca == Just posAtual = False
        | otherwise                               = livre posAtual minhocas


-- | Verifica se uma posição está livre de barris num dado estado.
livreDeBarris :: Posicao -> Estado -> Bool
livreDeBarris pos estado =
  all (\obj -> case obj of
                 Barril p _ -> p /= pos
                 _          -> True)
      (objetosEstado estado)

-----------------------------------------------------------------------------------












-------------------- funcoes que verificam se todas as posicoes se encontram livres de minhocas ou barris -----------------------------------------


-- | Verifica recursivamente se todas as posições de minhocas estão livres
verificaMinhocas :: [Minhoca] -> Estado -> Bool
verificaMinhocas [] _ = True
verificaMinhocas (minhoca:minhocas) estado =
    case posicaoMinhoca minhoca of
        Just posicao ->
            if livreDeMinhocas posicao (estado { minhocasEstado = minhocas })
            then verificaMinhocas minhocas estado
            else False
        Nothing -> verificaMinhocas minhocas estado

-- | Verifica recursivamente se todas as posições de barris estão livres
verificaBarris :: [Objeto] -> Estado -> Bool
verificaBarris [] _ = True
verificaBarris (barril:barris) estado
    | livreDeBarris (posicaoBarril barril) (estado { objetosEstado = barris })  = verificaBarris barris estado
    | otherwise                                                                 = False



---------------------------------------------------------------------------------------------------------










----------------------- funcoes de validacao -----------------------


-- | valida Minhoca
validaMinhoca :: Minhoca -> Estado -> Bool
validaMinhoca minhoca estado =
  case posicaoMinhoca minhoca of
    Nothing -> vidaMorta minhoca
    Just posicao -> dentroMapa posicao (mapaEstado estado)
                    && livreDeBarris posicao estado
                    && not (maybe False eTerrenoOpaco (terrenoNaPosicao posicao (mapaEstado estado)))
                    && livreDeMinhocas posicao (estado { minhocasEstado = filter (/= minhoca) (minhocasEstado estado) })
                    && vidaValida minhoca
                    && municoesValidas minhoca
                    && case terrenoNaPosicao posicao (mapaEstado estado) of
                        Just Agua -> vidaMorta minhoca
                        _         -> True


-- | valida Objeto
validaObjeto :: Objeto -> Estado -> Bool
validaObjeto objeto estado =
  case objeto of
    Barril posicao _ -> dentroMapa posicao (mapaEstado estado)
                        && not (maybe False eTerrenoOpaco (terrenoNaPosicao posicao (mapaEstado estado)))
                        && livreDeMinhocas posicao estado
                        && livreDeBarris posicao (estado { objetosEstado = filter (/= objeto) (objetosEstado estado) })

    Disparo posicao _ arma tempo dono -> dentroMapa posicao (mapaEstado estado)
                                         && not (maybe False eTerrenoOpaco (terrenoNaPosicao posicao (mapaEstado estado)))
                                         && livreDeMinhocas posicao estado
                                         && livreDeBarris posicao estado
                                         && tipoDisparoValido arma tempo dono (estado { objetosEstado = filter (/= objeto) (objetosEstado estado) })

    _ -> True -- casos que nao sejam nem barril nem disparo

-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo _ _ [] = False
minhocaTemDisparo arma num (obj:objs)
    | ehDisparo && tipoDisparo obj == arma && donoDisparo obj == num = True
    | otherwise = minhocaTemDisparo arma num objs
  where
    ehDisparo = case obj of
                  Disparo{} -> True
                  _ -> False

-- | Disparo Valido
tipoDisparoValido :: TipoArma -> Maybe Int -> NumMinhoca -> Estado -> Bool
tipoDisparoValido arma tempo dono estado =
     dono >= 0
  && dono < length (minhocasEstado estado)
  && not (minhocaTemDisparo arma dono (objetosEstado estado))
  && case arma of
       Jetpack          -> False
       Escavadora       -> False
       Bazuca           -> tempo == Nothing
       Mina             -> tempo == Nothing || tempo == Just 2
       Dinamite         -> tempo == Just 3

-- | Verifica se o estado é válido
validaEstado :: Estado -> Bool
validaEstado estado =
    validaMapa (mapaEstado estado)
    && verificaMinhocas (minhocasEstado estado) estado
    && verificaBarris [b | b@Barril{} <- objetosEstado estado] estado
    && all (`validaObjeto` estado) (objetosEstado estado)
    && all (`validaMinhoca` estado) (minhocasEstado estado)

----------------------------------------------













--  funcoes auxiliares à funcao validaMinhoca ----------

-- | função que verifica se a vida da minhoca é morta
vidaMorta :: Minhoca -> Bool
vidaMorta minhoca =
    case vidaMinhoca minhoca of
        Morta -> True
        _ -> False

-- | função que verifica se a vida da minhoca é valida
vidaValida :: Minhoca -> Bool
vidaValida minhoca =
    case vidaMinhoca minhoca of
        Viva n  -> n >= 0 && n <= 100
        Morta   -> True

-- | função que verifica se as munições da minhoca são válidas
municoesValidas :: Minhoca -> Bool
municoesValidas minhoca =
    if jetpackMinhoca minhoca < 0 then False
    else if escavadoraMinhoca minhoca < 0 then False
    else if bazucaMinhoca minhoca < 0 then False
    else if minaMinhoca minhoca < 0 then False
    else if dinamiteMinhoca minhoca < 0 then False
    else True
----------------------------------------------






















{-
 Apos ler o guiao do projeto percebemos de que as funcoes que se encontram acima
 percebemos que ainda que ao rodar "cabal run t1-feedback" com um estado teste, o nosso codigo aparecia como correto
 o unico problema é que nao respeitava as condicoes impostas pelo corpo docente da UC

 para que tal acontece necessitamos das seguintes funcoes:
-}


-- | Verifica se o mapa é retangular e não vazio
validaMapa :: Mapa -> Bool
validaMapa [] = False
validaMapa mapa =
    not (null mapa) --verifica se o mapa nao é vazio 
    && not (null (head mapa)) -- verifica se a primeira linha do mapa nao é vazia
    && all (\linha -> length linha == length (head mapa)) mapa --percorre todas as linhas do mapa e compara tamanhos.

-- | Verifica se uma posição está dentro dos limites do mapa
dentroMapa :: Posicao -> Mapa -> Bool
dentroMapa (l, c) mapa =
  not (null mapa)
  && l >= 0 && c >= 0
  && l < length mapa
  && c < length (head mapa)


-- | Obtém o terreno existente numa posição (se for válida)
terrenoNaPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoNaPosicao (l,c) m
  | dentroMapa (l,c) m  = Just ((m !! l) !! c)
  | otherwise           = Nothing

-- | Determina se o terreno é opaco (não atravessável) 
eTerrenoOpaco :: Terreno -> Bool --(retirada do ficheiro: Tarefa0_2025.hs)
eTerrenoOpaco terreno =
    case terreno of
        Terra -> True
        Pedra -> True
        _ -> False
------------------------------------------------------