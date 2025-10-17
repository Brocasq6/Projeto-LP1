-module Tarefa0_2025 where
    
import Labs2025


minhocaExemplo :: Minhoca
minhocaExemplo = Minhoca
    { posicaoMinhoca = Just (1,1)
    , vidaMinhoca = Viva 100
    , jetpackMinhoca = 3
    , escavadoraMinhoca = 2
    , bazucaMinhoca = 5
    , minaMinhoca = 1
    , dinamiteMinhoca = 0
}

mapaExemplo :: [[Terreno]]
mapaExemplo =
    [ [Terra, Agua, Agua]
    , [Terra, Terra, Terra]
    , [Pedra, Pedra, Pedra]
    ]
    

estadoExemplo = Estado
  { mapaEstado = undefined
  , objetosEstado = [Barril (2,3) False]
  , minhocasEstado = [Minhoca (Just (1,1)) (Viva 100) 0 0 0 0 0]
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
ePosicaoEstadoLivre pos estado = let 
        -- lista de posições ocupadas por barris
        posBarris = [posicaoBarril b | b@Barril{} <- objetosEstado estado]
        -- lista de posições ocupadas por minhocas vivas
        posMinhocas = [p | m <- minhocasEstado estado
                         , Viva _ <- [vidaMinhoca m]   -- apenas vivas
                         , Just p <- [posicaoMinhoca m]]

        -- posição está livre se não aparece em nenhuma das listas
    in pos `notElem` posBarris && pos `notElem` posMinhocas 


-- funcao que verifica se a posicao se encontra livre de minhicas
livreDeMinhocas :: Posicao -> Minhoca -> Bool

-- funcao que verifica se a posicao se encontra livre de barris


-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool 
minhocaTemDisparo _ _ [] = False -- se a lista de objetos estiver vazia, retorna False
minhocaTemDisparo arma num (objeto:objetos) =  case objeto of
            Disparo { tipoDisparo = a, donoDisparo = n } ->
             (a == arma && n == num) || minhocaTemDisparo arma num objetos
            _ -> minhocaTemDisparo arma num objetos

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

estadoExemplo2 =
  Estado
    { mapaEstado = undefined
    , objetosEstado = [Barril (2,3) False]
    , minhocasEstado = [Minhoca (Just (1,1)) (Viva 100) 0 0 0 0 0]
    }

novoEstado = adicionaObjeto (Disparo (3,3) Sul Bazuca Nothing 0) estadoExemplo2

