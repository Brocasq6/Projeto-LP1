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
-- | posição de um objeto
posObjeto :: Objeto -> Posicao
posObjeto (Barril p _)        = p
posObjeto (Disparo p _ _ _ _) = p

-- | próxima célula a partir de (linha,coluna)
proximaPosicao :: Direcao -> Posicao -> Posicao
proximaPosicao dir (l,c) =
  case dir of
    Norte -> (l-1, c)
    Sul -> (l+1, c)
    Este -> (l, c+1)      
    Oeste -> (l, c-1)
    Nordeste -> (l-1, c+1)
    Noroeste -> (l-1, c-1)
    Sudeste -> (l+1, c+1)
    Sudoeste -> (l+1, c-1)

-- tenta mover a minhoca n, respeitando mapa/colisões/terreno
-- | move APENAS a minhoca idx segundo as regras dos testes
moveMinhoca :: Direcao -> Estado -> NumMinhoca -> Minhoca
moveMinhoca dir est idx =
  let mapa = mapaEstado est
      ms = minhocasEstado est
      os = objetosEstado est
      m = ms !! idx
  in case posicaoMinhoca m of
       Nothing -> m
       Just p ->
         -- Se a célula atual for Ar/Terra/Pedra/Água, não sai do sítio (apenas "vira")
         case terrenoNaPosicao mapa p of
           Just Ar -> m   
           Just Terra -> m
           Just Pedra -> m
           Just Agua -> m
           Nothing -> m 
           _ ->
             -- (este ramo só ocorreria para outros terrenos; mantemos por segurança)
             let p' = proximaPosicao dir p
                 fora = not (dentroMapa p' mapa)
                 ocupMinh = [ q | (k,Minhoca{posicaoMinhoca=Just q}) <- zip [0..] ms, k /= idx ]
                 ocupObjs = [ case o of
                                Barril q _        -> q
                                Disparo q _ _ _ _ -> q
                            | o <- os ]
                 colide = p' `elem` ocupMinh || p' `elem` ocupObjs
             in if fora || colide
                  then m
                  else m { posicaoMinhoca = Just p' }  

-- | Verifica se uma posição está dentro do mapa.
dentroMapa :: Posicao -> Mapa -> Bool
dentroMapa (l,c) m =
  l >= 0 && c >= 0 && not (null m) &&
  l < length m && c < length (head m)

-- | Devolve o terreno numa dada posição do mapa.
terrenoNaPosicao :: Mapa -> Posicao -> Maybe Terreno
terrenoNaPosicao m (l,c)
  | l >= 0 && c >= 0 && l < length m && not (null m) && c < length (head m)
  = Just ((m !! l) !! c)
  | otherwise = Nothing

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
  let m' = moveMinhoca dir est n
  in est { minhocasEstado = atualizaLista n m' (minhocasEstado est) }

-- | A posição está livre para pisar? (Ar e sem objetos/minhocas)
posicaoLivre :: Posicao -> Estado -> Bool
posicaoLivre p (Estado m objs mins) =
  case terrenoNaPosicao m p of
    Just Ar ->
      not (any ((== p) . posObjeto) objs) &&
      not (any (== Just p) (map posicaoMinhoca mins))
    _ -> False


-- | Verifica se uma posição está no ar.
estaNoAr :: Posicao -> Mapa -> Bool
estaNoAr p m = terrenoNaPosicao m p == Just Ar


-- | Verifica se uma posição está na água.
estaNaAgua :: Posicao -> Mapa -> Bool
estaNaAgua p m = terrenoNaPosicao m p == Just Agua


-- | Verifica se há solo (Terra ou Pedra) imediatamente abaixo da posição dada.
temSoloAbaixo :: Posicao -> Mapa -> Bool
temSoloAbaixo (l,c) m =
  case terrenoNaPosicao m (l+1,c) of
    Just Terra -> True
    Just Pedra -> True
    _ -> False


-- | Verifica se uma minhoca pode agir (disparar ou mover).
podeAgir :: Estado -> Minhoca -> Bool
podeAgir est m =
  case posicaoMinhoca m of
    Just p -> estaNoAr p (mapaEstado est) && temSoloAbaixo p (mapaEstado est)
    _ -> False
  
-- | Substitui um elemento numa lista no índice dado.
substitui :: Int -> a -> [a] -> [a]
substitui idx novo xs =
  take idx xs ++ [novo] ++ drop (idx + 1) xs

-- | Move a minhoca i na direção dada, se as regras permitirem.
moveSeDer :: NumMinhoca -> Direcao -> Estado -> Estado
moveSeDer i dir e =
  case getMinhoca i e of
    Nothing -> e
    Just m  ->
      case posicaoMinhoca m of
        Nothing -> e
        Just p  ->
          let p'   = somaDir dir p
              mapa = mapaEstado e
          in case encontraPosicaoMatriz p' mapa of
               Nothing -> e                      -- fora do mapa
               Just t  ->
                 if t == Terra || t == Pedra
                   then e                        -- bloqueado
                   else setMinhocaPos i p' e     -- move
  where
    somaDir d (x,y) =
      case d of
        Norte -> (x, y-1)
        Sul   -> (x, y+1)
        Oeste -> (x-1, y)
        Este  -> (x+1, y)




--------------------------------------- funcoes relacionadas com a funcao efetuaJogadaDisparo -------------------------------------------------
-- | só estas armas geram objeto
armaDisparavel :: TipoArma -> Bool
armaDisparavel Bazuca = True
armaDisparavel Mina = True
armaDisparavel Dinamite = True
armaDisparavel _ = False  -- Jetpack/Escavadora não criam objeto

-- | tempo por arma
tempoDisparoDefault :: TipoArma -> Maybe Ticks
tempoDisparoDefault Bazuca = Nothing
tempoDisparoDefault Mina = Nothing
tempoDisparoDefault Dinamite = Just 3
tempoDisparoDefault _ = Nothing

-- | existe já um disparo igual (arma,dono)?
existeMesmoDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
existeMesmoDisparo arma dono =
  any (\o -> case o of
         Disparo _ _ a _ d -> a == arma && d == dono
         _ -> False)

-- | Verifica se uma minhoca tem munição para a arma.
temMunicao :: TipoArma -> Minhoca -> Bool
temMunicao arma m = case arma of
  Jetpack -> jetpackMinhoca m > 0
  Escavadora -> escavadoraMinhoca m > 0
  Bazuca -> bazucaMinhoca m > 0
  Mina -> minaMinhoca m > 0
  Dinamite -> dinamiteMinhoca m > 0

-- | Consome munição de uma minhoca ao disparar uma arma.
consomeMunicao :: TipoArma -> Minhoca -> Minhoca
consomeMunicao arma m = case arma of
  Jetpack -> m { jetpackMinhoca = jetpackMinhoca m - 1 }
  Escavadora -> m { escavadoraMinhoca = escavadoraMinhoca m - 1 }
  Bazuca -> m { bazucaMinhoca = bazucaMinhoca m - 1 }
  Mina -> m { minaMinhoca = minaMinhoca m - 1 }
  Dinamite -> m { dinamiteMinhoca = dinamiteMinhoca m - 1 }

-- | escolhe a posição inicial do disparo (na frente se der; senão na própria)
posInicialDisparo :: Estado -> Direcao -> Minhoca -> Posicao
posInicialDisparo est dir m =
  case posicaoMinhoca m of
    Nothing -> error "Minhoca sem posição"
    Just p ->
      let mapa = mapaEstado est
          pF = proximaPosicao dir p
          ok q = dentroMapa q mapa
              && maybe True (/= Pedra) (terrenoNaPosicao mapa q)
      in if ok pF then pF else p

-- | Cria um objeto disparo a partir do estado, tipo de arma, direção, número da minhoca e a própria minhoca.
criaDisparo :: Estado -> TipoArma -> Direcao -> NumMinhoca -> Minhoca -> Objeto
criaDisparo est arma dir dono m =
  let p0 = posInicialDisparo est dir m
  in Disparo p0 dir arma (tempoDisparoDefault arma) dono


-- | Efetua a jogada de disparo, criando o objeto disparo e atualizando a minhoca.
setTerreno :: Posicao -> Terreno -> Mapa -> Mapa
setTerreno (l,c) t m =
  let linha = m !! l
      linha' = take c linha ++ [t] ++ drop (c+1) linha
  in  take l m ++ [linha'] ++ drop (l+1) m


-- | Atualiza uma minhoca numa lista no índice dado.
atualizaMinhocaIdx :: Int -> Minhoca -> [Minhoca] -> [Minhoca]
atualizaMinhocaIdx i w ws = take i ws ++ [w] ++ drop (i+1) ws

-- | Atualiza um elemento numa lista no índice dado.
atualizaLista :: Int -> a -> [a] -> [a]
atualizaLista i novo l = take i l ++ [novo] ++ drop (i + 1) l 


-- | Verifica se o terreno numa dada posição é bloqueado (Terra ou Pedra).
terrenoBloqueado :: Posicao -> Mapa -> Bool
terrenoBloqueado p mapa =
  case terrenoNaPosicao mapa p of  -- <- troca a ordem aqui
    Just Terra -> True
    Just Pedra -> True
    _ -> False

-- | Efetua uma jogada de jetpack por parte de uma minhoca, atualizando o estado.
jogaJetpack :: NumMinhoca -> Direcao -> Estado -> Estado
jogaJetpack i dir e@(Estado m _ mins)
  | i < 0 || i >= length mins = e
  | Nothing <- posicaoMinhoca w = e
  | not (temMunicao Jetpack w) = e
  | Just p <- posicaoMinhoca w
  , terrenoBloqueado p m = e        
  | not (dentroMapa p' m) = e
  | not (posicaoLivre p' e) = e
  | otherwise =
      let w' = (consomeMunicao Jetpack w) { posicaoMinhoca = Just p' }
      in  e { minhocasEstado = atualizaMinhocaIdx i w' mins }
  where
    w  = mins !! i
    Just p = posicaoMinhoca w
    p' = proximaPosicao dir p

-- | Efetua uma jogada de escavação por parte de uma minhoca, atualizando o estado.
jogaEscavadora :: NumMinhoca -> Direcao -> Estado -> Estado
jogaEscavadora i dir e@(Estado m objs mins)
  | i < 0 || i >= length mins = e
  | Nothing <- posicaoMinhoca w = e
  | not (temMunicao Escavadora w) = e
  | Just p <- posicaoMinhoca w
  ,  terrenoBloqueado p m = e        
  | not (dentroMapa p' m) = e
  | ocupadoPorAlgo = e
  | otherwise =
      case terrenoNaPosicao m p' of
        Just Pedra -> e
        Just Agua -> e
        Just Terra ->
          let m'= setTerreno p' Ar m
              w'= (consomeMunicao Escavadora w) { posicaoMinhoca = Just p' }
          in  Estado m' objs (atualizaMinhocaIdx i w' mins)
        Just Ar ->
          let w' = (consomeMunicao Escavadora w) { posicaoMinhoca = Just p' }
          in  e { minhocasEstado = atualizaMinhocaIdx i w' mins }
        _ -> e
  where
    w  = mins !! i
    Just p = posicaoMinhoca w
    p' = proximaPosicao dir p
    ocupadoPorAlgo =
      any ((== p') . posObjeto) objs ||
      any (== Just p') (map posicaoMinhoca mins)

-- | Efetua uma jogada de disparo por parte de uma minhoca, atualizando o estado.
efetuaJogadaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
efetuaJogadaDisparo n arma dir est =
  let ms = minhocasEstado est
      m = ms !! n
      mapa = mapaEstado est
  in case posicaoMinhoca m of
       Nothing -> est
       Just _ ->
         -- Não dispara se está em Pedra/Terra/Água, enterrada, sem suporte, etc.
         if not (podeAgir est m) then est
         else if not (armaDisparavel arma) then est
         else if not (temMunicao arma m) then est
         else if existeMesmoDisparo arma n (objetosEstado est) then est
         else
           let m'  = consomeMunicao arma m
               obj = criaDisparo est arma dir n m'
           in est { objetosEstado  = obj : objetosEstado est
                  , minhocasEstado = atualizaLista n m' ms }

----------------------------------------------------------------------------------------------------------------

-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada i (Move dir) e = moveSeDer i dir e
efetuaJogada i (Dispara arma dir) e =
  case arma of
    Jetpack -> jogaJetpack i dir e
    Escavadora -> jogaEscavadora i dir e
    _ -> efetuaJogadaDisparo i arma dir e  -- bazuca/mina/dinamite

