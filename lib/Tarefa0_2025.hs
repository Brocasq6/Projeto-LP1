{-|
Module      : Tarefa0_2025
Description : Funções auxiliares.

Módulo que define funções auxiliares que serão úteis na resolução do trabalho prático de LI1\/LP1 em 2025\/26.
-}

-- | Este módulo 
module Tarefa0_2025 where
    
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
    

-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca Jetpack minhoca = jetpackMinhoca minhoca 
encontraQuantidadeArmaMinhoca Escavadora minhoca = escavadoraMinhoca minhoca
encontraQuantidadeArmaMinhoca Bazuca minhoca =  bazucaMinhoca minhoca
encontraQuantidadeArmaMinhoca Mina minhoca = minaMinhoca minhoca
encontraQuantidadeArmaMinhoca Dinamite minhoca = dinamiteMinhoca minhoca

-- | Atualia a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca arma minhoca novasBalas =
    case arma of
        Jetpack -> minhoca { jetpackMinhoca = jetpackMinhoca minhoca + novasBalas } -- atualizo apenas o campo da municao da arma que estou a usar para a nova quantidade de balas?
        Escavadora -> minhoca { escavadoraMinhoca = escavadoraMinhoca minhoca + novasBalas }
        Bazuca -> minhoca { bazucaMinhoca = bazucaMinhoca minhoca + novasBalas }
        Mina -> minhoca { minaMinhoca = minaMinhoca minhoca + novasBalas }
        Dinamite -> minhoca { dinamiteMinhoca = dinamiteMinhoca minhoca + novasBalas }

atualizaQuantidadeArmaMinhoca_ :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca_ arma minhoca novasBalas =
    case arma of
        Jetpack -> minhoca { jetpackMinhoca = novasBalas } -- atualizo apenas o campo da municao da arma que estou a usar para a nova quantidade de balas?
        Escavadora -> minhoca { escavadoraMinhoca = novasBalas }
        Bazuca -> minhoca { bazucaMinhoca = novasBalas }
        Mina -> minhoca { minaMinhoca = novasBalas }
        Dinamite -> minhoca { dinamiteMinhoca = novasBalas }



-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel terreno =
    case terreno of
        Terra -> True
        _ -> False

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco terreno =
    case terreno of
        Terra -> True
        Pedra -> True
        _ -> False
         

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre pos mapa =  
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> not (eTerrenoOpaco terreno)     -- se a posicao for Agua ou Ar, retorna True
        Nothing      -> False                           -- se a posicao for terra ou pedra, retorna False

-- vamos ter de verificar se a posicao existe no mapa, e se o terreno nessa posicao é opaco ou nao

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.

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

estadoExemplo = Estado
  { mapaEstado = undefined
  , objetosEstado = [Barril (2,3) False]
  , minhocasEstado = [Minhoca (Just (1,1)) (Viva 100) 0 0 0 0 0]
  }


-- funcao que verifica se a posicao se encontra livre de minhicas

-- funcao que verifica se a posicao se encontra livre de barris


-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool 
minhocaTemDisparo _ _ [] = False -- se a lista de objetos estiver vazia, retorna False
minhocaTemDisparo arma num (objeto:objetos) =  case objeto of
            Disparo { tipoDisparo = a, donoDisparo = n } ->
             (a == arma && n == num) || minhocaTemDisparo arma num objetos
            _ -> minhocaTemDisparo arma num objetos

-- vamos ter de verificar se o objeto é um disparo, e se o disparo é da arma e da minhoca que estamos a procurar

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
destroiPosicao :: Posicao -> Mapa -> Mapa -- vamos ter de verificar se o terreno é destrutivel, se for, atualizamos a posicao para Ar
destroiPosicao pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just Terra -> atualizaPosicaoMatriz pos Ar mapa  
        Nothing -> mapa

-- Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado =
    estado { objetosEstado = obj : objetosEstado estado }

estadoExemplo2 =
  Estado
    { mapaEstado = undefined
    , objetosEstado = [Barril (2,3) False]
    , minhocasEstado = [Minhoca (Just (1,1)) (Viva 100) 0 0 0 0 0]
    }

novoEstado = adicionaObjeto (Disparo (3,3) (1,0) Bazuca Nothing 0) estadoExemplo2

