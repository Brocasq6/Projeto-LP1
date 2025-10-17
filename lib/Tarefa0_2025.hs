module Tarefa0_2025 where
    
import Labs2025


minhoca1 :: Minhoca
minhoca1 = Minhoca
    { posicaoMinhoca = Just (1,1)
    , vidaMinhoca    = Viva 100
    , jetpackMinhoca = 3
    , escavadoraMinhoca = 2
    , bazucaMinhoca  = 5
    , minaMinhoca    = 1
    , dinamiteMinhoca = 0
    }

minhoca2 :: Minhoca
minhoca2 = Minhoca
    { posicaoMinhoca = Just (2,2)
    , vidaMinhoca    = Viva 50
    , jetpackMinhoca = 1
    , escavadoraMinhoca = 0
    , bazucaMinhoca  = 2
    , minaMinhoca    = 0
    , dinamiteMinhoca = 1
    }

mapaExemplo :: Mapa
mapaExemplo =
    [ [Terra, Agua, Ar]
    , [Pedra, Terra, Terra]
    , [Ar, Agua, Pedra]
    ]

barril1 :: Objeto
barril1 = Barril (0,2) False

disparo1 :: Objeto
disparo1 = Disparo
    { posicaoDisparo = (1,2)
    , direcaoDisparo = Sul
    , tipoDisparo = Bazuca
    , tempoDisparo = Just 3
    , donoDisparo = 0
    }


-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca arma minhoca =
    case arma of
        Jetpack -> jetpackMinhoca minhoca
        Escavadora -> escavadoraMinhoca minhoca
        Bazuca -> bazucaMinhoca minhoca
        Mina -> minaMinhoca minhoca
        Dinamite -> dinamiteMinhoca minhoca

-- | Atualia a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca_ :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca_ arma minhoca novasBalas =
    case arma of
        Jetpack -> minhoca { jetpackMinhoca = novasBalas } -- atualizo apenas o campo da municao da arma que estou a usar para a nova quantidade de balas?
        Escavadora -> minhoca { escavadoraMinhoca = novasBalas }
        Bazuca -> minhoca { bazucaMinhoca = novasBalas }
        Mina -> minhoca { minaMinhoca = novasBalas }
        Dinamite -> minhoca { dinamiteMinhoca = novasBalas }

-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel terreno =
    case terreno of
        Terra -> True
        _ -> False

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco terreno =
    case terreno of
        Terra -> True
        Pedra -> True
        _ -> False
         
-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre pos mapa =  
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> not (eTerrenoOpaco terreno)     -- se a posicao for Agua ou Ar, retorna True
        Nothing      -> False                           -- se a posicao for terra ou pedra, retorna False

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre pos estado
    | livreDeMinhocas pos estado && livreDeBarris pos estado = True
    | otherwise = False

livreDeMinhocas :: Posicao -> Estado -> Bool
livreDeMinhocas pos estado = livre pos (minhocasEstado estado)
  where
    livre _ [] = True  -- lista vazia -> posição livre
    livre pos (m:ms)
        | posicaoMinhoca m == Just pos = False -- a posição já se encontra ocupada
        | otherwise = livre pos ms              -- verifica o resto da lista

livreDeBarris :: Posicao -> Estado -> Bool
livreDeBarris pos estado = livre pos (objetosEstado estado)
  where
    livre _ [] = True  -- lista vazia -> posição livre
    livre pos (o:os)
        | ehBarril o && posicaoBarril o == pos = False  -- posição ocupada por barril
        | otherwise = livre pos os                    -- verifica o resto da lista

    -- Função auxiliar para identificar se o objeto é um Barril
    ehBarril (Barril _ _) = True
    ehBarril _ = False

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

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
destroiPosicao :: Posicao -> Mapa -> Mapa -- vamos ter de verificar se o terreno é destrutivel, se for, atualizamos a posicao para Ar
destroiPosicao pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just Terra -> atualizaPosicaoMatriz pos Ar mapa  
        Nothing -> mapa

-- Adiciona um novo objeto a um estado.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado =
    estado { objetosEstado = obj : objetosEstado estado }
