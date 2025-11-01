{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025



{-

efetuaJogada
 |
 | ── efetuaJogadaMove
 │      |── moveMinhoca
 │      │       |── proximaPosicao
 │      │       |── terrenoNaPosicao
 │      │       |── aplicaEfeitoTerreno
 │      |── atualizaLista
 │
  ── efetuaJogadaDisparo
        |── temMunicao
        |── consomeMunicao
        |── criaDisparo
        |── atualizaLista

-}


--------------------------------------- funcoes relacionadas com a funcao efetuaJogadaMove -------------------------------------------------
-- posição de um objeto
posObjeto :: Objeto -> Posicao
posObjeto (Barril p _)            = p
posObjeto (Disparo p _ _ _ _)     = p

-- próxima célula a partir de (linha,coluna)
proximaPosicao :: Direcao -> Posicao -> Posicao
proximaPosicao Norte    (l,c) = (l-1, c)
proximaPosicao Sul      (l,c) = (l+1, c)
proximaPosicao Este     (l,c) = (l, c+1)
proximaPosicao Oeste    (l,c) = (l, c-1)
proximaPosicao Nordeste (l,c) = (l-1, c+1)
proximaPosicao Noroeste (l,c) = (l-1, c-1)
proximaPosicao Sudeste  (l,c) = (l+1, c+1)
proximaPosicao Sudoeste (l,c) = (l+1, c-1)

-- tenta mover a minhoca n, respeitando mapa/colisões/terreno
moveMinhoca :: Direcao -> Estado -> NumMinhoca -> Minhoca
moveMinhoca dir est idx =
  let mapa = mapaEstado est
      ms   = minhocasEstado est
      objs = objetosEstado est
      m    = ms !! idx
  in case posicaoMinhoca m of
       Nothing -> m
       Just p  ->
         let p'       = proximaPosicao dir p
             fora     = not (dentroMapa p' mapa)
             ocupMinh = [ q | (k,Minhoca{posicaoMinhoca=Just q}) <- zip [0..] ms, k /= idx ]
             ocupObjs = map posObjeto objs
             colide   = p' `elem` ocupMinh || p' `elem` ocupObjs
             t'       = terrenoNaPosicao mapa p'
         in if fora || colide
              then m
              else case t' of
                     Pedra -> m
                     Agua  -> m { posicaoMinhoca = Just p', vidaMinhoca = Morta }
                     _     -> m { posicaoMinhoca = Just p' }

dentroMapa :: Posicao -> Mapa -> Bool
dentroMapa (l,c) m =
  l >= 0 && c >= 0 && l < length m && not (null m) && c < length (head m)

-- | Devolve o terreno numa dada posição do mapa.
terrenoNaPosicao :: Mapa -> Posicao -> Terreno
terrenoNaPosicao m (l,c) = (m !! l) !! c

-- | Aplica o efeito do terreno na minhoca.
aplicaEfeitoTerreno :: Minhoca -> Posicao -> Terreno -> Minhoca
aplicaEfeitoTerreno m pos terreno =
  case terreno of
    Ar -> m { posicaoMinhoca = Just pos }
    Terra -> m { posicaoMinhoca = Just pos }
    Pedra -> m  -- não se move
    Agua -> m { vidaMinhoca = Morta, posicaoMinhoca = Just pos }

-- | Atualiza a lista de minhocas com a nova minhoca na posição dada.
efetuaJogadaMove :: NumMinhoca -> Direcao -> Estado -> Estado
efetuaJogadaMove n dir est =
  let nova = moveMinhoca dir est n
  in est { minhocasEstado = atualizaLista n nova (minhocasEstado est) }

--------------------------------------- funcoes relacionadas com a funcao efetuaJogadaDisparo -------------------------------------------------
-- só estas armas geram objeto
armaDisparavel :: TipoArma -> Bool
armaDisparavel Bazuca   = True
armaDisparavel Mina     = True
armaDisparavel Dinamite = True
armaDisparavel _        = False  -- Jetpack/Escavadora não criam objeto

-- tempo por arma
tempoDisparoDefault :: TipoArma -> Maybe Ticks
tempoDisparoDefault Bazuca   = Nothing
tempoDisparoDefault Mina     = Nothing
tempoDisparoDefault Dinamite = Just 3
tempoDisparoDefault _        = Nothing

-- existe já um disparo igual (arma,dono)?
existeMesmoDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
existeMesmoDisparo arma dono =
  any (\o -> case o of
         Disparo _ _ a _ d -> a == arma && d == dono
         _                 -> False)

temMunicao :: TipoArma -> Minhoca -> Bool
temMunicao arma m = case arma of
  Jetpack    -> jetpackMinhoca m    > 0
  Escavadora -> escavadoraMinhoca m > 0
  Bazuca     -> bazucaMinhoca m     > 0
  Mina       -> minaMinhoca m       > 0
  Dinamite   -> dinamiteMinhoca m   > 0

consomeMunicao :: TipoArma -> Minhoca -> Minhoca
consomeMunicao arma m = case arma of
  Jetpack    -> m { jetpackMinhoca    = jetpackMinhoca m - 1 }
  Escavadora -> m { escavadoraMinhoca = escavadoraMinhoca m - 1 }
  Bazuca     -> m { bazucaMinhoca     = bazucaMinhoca m - 1 }
  Mina       -> m { minaMinhoca       = minaMinhoca m - 1 }
  Dinamite   -> m { dinamiteMinhoca   = dinamiteMinhoca m - 1 }

criaDisparo :: TipoArma -> Direcao -> NumMinhoca -> Minhoca -> Objeto
criaDisparo arma dir dono m =
  case posicaoMinhoca m of
    Nothing -> error "Minhoca fora do mapa"
    Just p  -> Disparo p dir arma (tempoDisparoDefault arma) dono

-- | Atualiza um elemento numa lista no índice dado.
atualizaLista :: Int -> a -> [a] -> [a]
atualizaLista i novo l = take i l ++ [novo] ++ drop (i + 1) l 

-- | Efetua uma jogada de disparo por parte de uma minhoca, atualizando o estado.
efetuaJogadaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
efetuaJogadaDisparo n arma dir est =
  let ms = minhocasEstado est
      m  = ms !! n
  in case posicaoMinhoca m of
       Nothing -> est
       Just _  ->
         if not (armaDisparavel arma) then est
         else if not (temMunicao arma m) then est
         else if existeMesmoDisparo arma n (objetosEstado est) then est
         else
           let m'     = consomeMunicao arma m
               obj    = criaDisparo arma dir n m'
           in est { objetosEstado  = obj : objetosEstado est
                  , minhocasEstado = atualizaLista n m' ms }

----------------------------------------------------------------------------------------------------------------

-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n jogada estado =
  case jogada of
    Move dir -> efetuaJogadaMove n dir estado
    Dispara arma dir -> efetuaJogadaDisparo n arma dir estado

