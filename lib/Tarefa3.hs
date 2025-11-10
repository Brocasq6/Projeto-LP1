{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Tarefa3 where 
import Labs2025
import Tarefa1 hiding (terrenoNaPosicao) 
import Data.Either (partitionEithers)

-- | Tipo de dado para representar danos em posições.
type Dano = Int
-- | Tipo de dado para representar uma lista de danos em várias posições.
type Danos = [(Posicao,Dano)]

-- | Função principal da Tarefa 3. Avanço o estado do jogo um tick.
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto $ e { minhocasEstado = minhocas' }) (zip [0..] objetos)
    e' = Estado mapa objetos' minhocas'


-- | Para um dado estado, dado o índice de uma minhoca na lista de minhocas e o estado dessa minhoca, retorna o novo estado da minhoca no próximo tick.
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca estado _ minhoca
  | minhocaMorta minhoca = minhoca
  | otherwise =
      case posicaoMinhoca minhoca of
        Nothing -> minhoca
        Just posicao -> atualizaPosicaoGravidade estado minhoca posicao

-- | Atualiza a posição de uma minhoca aplicando a gravidade.
atualizaPosicaoGravidade :: Estado -> Minhoca -> Posicao -> Minhoca
atualizaPosicaoGravidade estado minhoca posicao = 
  case terrenoNaPosicao abaixo mapa of 
    Nothing -> mataMinhoca minhoca Nothing
    Just Agua -> mataMinhoca minhoca (Just abaixo)
    Just Ar -> minhoca { posicaoMinhoca = Just (aplicaGravidade abaixo mapa)}
    _ -> minhoca
  where
    mapa = mapaEstado estado
    abaixo = (fst posicao + 1 , snd posicao)
  
-- | verifica se uma mihoca está morta
minhocaMorta :: Minhoca -> Bool
minhocaMorta minhoca =
    case vidaMinhoca minhoca of
       Morta -> True
       Viva _ -> False

-- | mata uma minhoca, definindo a sua vida como Morta e a sua posição como Nothing
mataMinhoca :: Minhoca -> Maybe Posicao -> Minhoca
mataMinhoca minhoca pos = minhoca { vidaMinhoca = Morta, posicaoMinhoca = pos }



-- | Posição de um objeto
posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril  p _) = p
posicaoObjeto (Disparo p _ _ _ _)  = p

-- | Retorna o terreno na posição dada do mapa.
terrenoNaPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoNaPosicao (l,c) m
  | l >= 0 && c >= 0 && l < length m && not (null m) && c < length (head m)
  = Just ((m !! l) !! c)
  | otherwise = Nothing

-- | Verifica se uma posição está no ar ou na água.
estaNoArOuAgua :: Posicao -> Mapa -> Bool   
estaNoArOuAgua posicao mapa =
    case terrenoNaPosicao posicao mapa of
        Just Agua -> True
        Just Ar -> True
        _ -> False

-- | Verifica se uma posição é válida (dentro do mapa e não ocupada por terreno opaco).
posicaoValida :: Posicao -> Mapa -> Bool    
posicaoValida posicao mapa =
    case terrenoNaPosicao posicao mapa of
        Just Pedra -> False
        Just Terra -> False
        Nothing -> False
        _ -> True
  
-- | Aplica a gravidade a uma posição, retornando a nova posição após a aplicação da gravidade.
aplicaGravidade :: Posicao -> Mapa -> Posicao   
aplicaGravidade (linha, coluna) mapa
  | dentroMapa abaixo mapa && estaNoArOuAgua abaixo mapa = aplicaGravidade abaixo mapa
  | otherwise = (linha, coluna)
  where
    abaixo = (linha + 1, coluna)


{-
 |── avancaMinhoca
 |     |── posicaoMinhoca
 |     |── terrenoNaPosicao
 |     |── estaNoArOuAgua
 |     |── posicaoValida
 |     └── aplicaGravidade
-}





-- | Para um dado estado, dado o índice de um objeto na lista de objetos e o estado desse objeto, retorna o novo estado do objeto no próximo tick ou, caso o objeto expluda, uma lista de posições afetadas com o dano associado.
avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto estado _ obj =
  case tipoDisparo obj of
    Bazuca     -> avancaBazuca   estado obj
    Mina       -> avancaMina     estado obj
    Dinamite   -> avancaDinamite estado obj
    Jetpack    -> Left obj
    Escavadora -> Left obj

-- | move APENAS a minhoca idx segundo as regras dos testes
avancaBarril :: Estado -> Objeto -> Either Objeto Danos
avancaBarril est (Barril p True)  = Right (geraExplosao p 5)
avancaBarril est (Barril p flag)
  | estaNoArOuAgua p (mapaEstado est) = Left (Barril p True)
  | otherwise                         = Left (Barril p flag)
avancaBarril _ o = Left o

-- | move APENAS a minhoca idx segundo as regras dos testes
avancaDisparo :: Estado -> Objeto -> Either Objeto Danos
avancaDisparo estado obj =
  case tipoDisparo obj of
    Bazuca     -> avancaBazuca   estado obj
    Mina       -> avancaMina     estado obj
    Dinamite   -> avancaDinamite estado obj
    Jetpack    -> Left obj
    Escavadora -> Left obj

data Hit = Livre | Bate | Fora
  deriving (Eq, Show)

colisaoBazuca :: Posicao -> Mapa -> [Objeto] -> Hit
colisaoBazuca p mapa objs
  | not (dentroMapa p mapa) = Fora
  | opaco terreno           = Bate
  | any ((== p) . posicaoObjeto) objs = Bate
  | otherwise               = Livre
  where
    terreno = terrenoNaPosicao p mapa
    opaco (Just Terra) = True
    opaco (Just Pedra) = True
    -- se o teu terrenoNaPosicao devolve Nothing apenas fora do mapa,
    -- já tratámos acima com Fora; aqui fica False.
    opaco _            = False

-- | move APENAS a minhoca idx segundo as regras dos testes
avancaBazuca :: Estado -> Objeto -> Either Objeto Danos
avancaBazuca estado obj =
  let mapa    = mapaEstado estado
      pos0    = posicaoDisparo obj
      novaPos = moveDisparo (direcaoDisparo obj) pos0
  in if verificaColisao novaPos mapa
        then Right (geraExplosao novaPos 5)
        else Left  (obj { posicaoDisparo = novaPos })

-- | Avança o estado de uma Mina.
avancaMina :: Estado -> Objeto -> Either Objeto Danos
avancaMina est o0 =
  case tempoDisparo o0 of
    Just 0 ->
      Right (geraExplosao (posicaoObjeto o0) 5)

    Just t ->
      let o1 = quedaMina est o0
      in Left (o1 { tempoDisparo = Just (t-1) })

    Nothing ->
      let o1 = quedaMina est o0
          o2 = if deveAtivarMina est o1
                 then o1 { tempoDisparo = Just 2 }  -- ativa (sem decrementar agora)
                 else o1
      in Left o2

-- | move APENAS a minhoca idx segundo as regras dos testes
avancaDinamite :: Estado -> Objeto -> Either Objeto Danos
avancaDinamite est obj =
  case tempoDisparo obj of
    Just 0 -> Right (geraExplosao (posicaoObjeto obj) 7)
    Just t ->
      let nova = aplicaGravidade (posicaoObjeto obj) (mapaEstado est)
      in Left (obj { posicaoDisparo = nova, tempoDisparo = Just (t-1) })
    Nothing ->
      -- se dinâmite “armada” sem tempo, mantém/ajusta (se não usas este caso, podes deixar como Left obj)
      let nova = aplicaGravidade (posicaoObjeto obj) (mapaEstado est)
      in Left (obj { posicaoDisparo = nova })

-- | move APENAS a minhoca idx segundo as regras dos testes
moveDisparo :: Direcao -> Posicao -> Posicao
moveDisparo direcao (l,c) = 
    case direcao of
        Norte -> (l-1,c)
        Sul -> (l+1,c)
        Este -> (l,c+1)
        Oeste -> (l,c-1)
        Nordeste -> (l-1,c+1)
        Noroeste -> (l-1,c-1)
        Sudeste -> (l+1,c+1)
        Sudoeste -> (l+1,c-1)

-- | verifica se ha uma colisao na posicao dada
verificaColisao :: Posicao -> Mapa -> [Objeto] -> Bool
verificaColisao p mapa objs =
  case terrenoNaPosicao p mapa of
    Nothing    -> True
    Just Pedra -> True
    Just Terra -> True
    _          -> any ((== p) . posicaoObjeto) objs

-- | faz a contagem decrescente do tempo de vida do disparo
contaTempo :: Objeto -> Maybe Int
contaTempo obj =
  case tempoDisparo obj of
    Nothing -> Nothing
    Just t  -> Just (t - 1)

-- | funcao que ativa uma mina
ativaMina :: Estado -> Objeto -> Objeto
ativaMina estado mina@(Disparo p _ Mina t dono) =
  let dentroArea m =
        case posicaoMinhoca m of
          Just q -> estaNaAreaExplosao q p 3   -- mesma área da explosão
          _      -> False
      haInimigo = any id
                [ dentroArea m
                | (ix,m) <- zip [0..] (minhocasEstado estado)
                , ix /= dono
                ]
  in if t == Nothing && haInimigo
        then mina { tempoDisparo = Just 2 }
        else mina
ativaMina _ o = o

quedaMina :: Estado -> Objeto -> Objeto
quedaMina est o =
  let m   = mapaEstado est
      pos = posicaoObjeto o
  in case terrenoNaPosicao pos m of
       Just Ar   -> o { posicaoDisparo = aplicaGravidade pos m, direcaoDisparo = Norte }
       Just Agua -> o { posicaoDisparo = aplicaGravidade pos m, direcaoDisparo = Norte }
       _         -> o

deveAtivarMina :: Estado -> Objeto -> Bool
deveAtivarMina est mina =
  any (\(i,m) ->
        i /= donoDisparo mina &&
        maybe False (\p -> estaNaAreaExplosao p (posicaoObjeto mina) 5) (posicaoMinhoca m)
      ) (zip [0..] (minhocasEstado est))


-- | verifica se uma posicao esta na area de explosao
estaNaAreaExplosao :: Posicao -> Posicao -> Int -> Bool
estaNaAreaExplosao (x1,y1) (x2,y2) diametro = 
    abs (x1 - x2) <= raio && abs (y1 - y2) <= raio
    where
        raio = diametro `div` 2

-- | gera uma explsao numa posicao com um dado dano
geraExplosao :: Posicao -> Dano -> Danos
geraExplosao (cx, cy) d =
  centro <> ortogonais <> diagonais
  where
    centro = [((cx, cy), d * 10)]

    kMaxOrt  = (d - 1) `div` 2
    kMaxDiag = (d - 1) `div` 3

    -- cruz (N, S, E, O)
    ortogonais =
      [ ((cx + dx * k, cy + dy * k), v)
      | k <- [1 .. kMaxOrt]
      , (dx,dy) <- [(1,0), (-1,0), (0,1), (0,-1)]
      , let v = (d - 2 * k) * 10
      , v > 0
      ]

    -- diagonais (NE, NO, SE, SO)
    diagonais =
      [ ((cx + dx * k, cy + dy * k), v)
      | k <- [1 .. kMaxDiag]
      , (dx,dy) <- [(1,1), (1,-1), (-1,1), (-1,-1)]
      , let v = (d - 3 * k) * 10
      , v > 0
      ]


-- | cria uma lista de danos para uma dada posicao e dano
criaListaDanos :: Posicao -> Dano -> Danos
criaListaDanos posicao dano = [(posicao,dano)]

-- | definicao do data type TipoObjeto
data TipoObjeto = OBarril | ODisparo deriving (Eq, Show)

tipoObjeto :: Objeto -> TipoObjeto
tipoObjeto (Barril {})  = OBarril
tipoObjeto (Disparo {}) = ODisparo

{-
 |- avancaObjeto
 |     |- tipoObjeto
 |     |     |- Barril
 |     |     |   |- verificaExplosao
 |     |     |   |- estaNoArOuAgua
 |     |     |
 |     |     |- Disparo
 |     |         |- moveDisparo
 |     |         |- verificaColisao
 |     |         |- contaTempo
 |     |         |- ativaMina
 |     |         |- geraExplosao
 |     |
 |     |- criaListaDanos
 |--
 -}

-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos estado =
  estado
    { minhocasEstado = calculaDanoMinhocas danos (minhocasEstado estado)
    , mapaEstado     = atualizaMapa        danos (mapaEstado estado)
    , objetosEstado  = atualizaObjetos     danos (objetosEstado estado)
    }

-- | aplica os danos a cada minhoca
calculaDanoMinhocas :: Danos -> [Minhoca] -> [Minhoca]
calculaDanoMinhocas danos = map aplica
  where
    -- dado uma minhoca, vê se a sua posição está numa célula com dano;
    -- se sim, aplica (somando danos repetidos nessa célula)
    aplica :: Minhoca -> Minhoca
    aplica m =
      case posicaoMinhoca m of
        Nothing -> m
        Just pos ->
          case verificaPosicaoAfetada pos danos of
            Nothing -> m
            Just dano -> reduzVidaOuMata m dano

-- | devolve o total de dano naquela posição, se existir
verificaPosicaoAfetada :: Posicao -> Danos -> Maybe Dano
verificaPosicaoAfetada pos ds =
  let soma = sum [ d | (p,d) <- ds, p == pos ]
  in if soma == 0 then Nothing else Just soma

-- | reduz vida; se chegar a zero ou menos, mata (mantém posição)
reduzVidaOuMata :: Minhoca -> Dano -> Minhoca
reduzVidaOuMata m d =
  case vidaMinhoca m of
    Morta -> m
    Viva hp -> if d >= hp
               then m { vidaMinhoca = Morta }
               else m { vidaMinhoca = Viva (hp - d) }

-- | remove/transforma terreno nas posições atingidas (simples: remove)
atualizaMapa :: Danos -> Mapa -> Mapa
atualizaMapa danos mapa =
  foldl remover mapa (map fst danos)
  where
    remover m pos = removeTerrenoAtingido pos m

-- | funcao que atualiza os objetos
atualizaObjetos :: Danos -> [Objeto] -> [Objeto]
atualizaObjetos danos =
  filter manter
  where
    atingiu p = any (\(q,d) -> q == p && d > 0) danos
    manter o =
      let p = posicaoObjeto o
      in p /= (-1,-1) && not (atingiu p)

-- | funcao que remove as partes do terreno que foram atingido
removeTerrenoAtingido :: Posicao -> Mapa -> Mapa
removeTerrenoAtingido (l, c) mapa
  | not (dentroMapa (l, c) mapa) = mapa
  | otherwise = atualizaCel mapa (l, c) Ar

-- | funcao que atualiza uma celula de um mapa
atualizaCel :: Mapa -> Posicao -> Terreno -> Mapa
atualizaCel mapa (l, c) novoTerreno =
  take l mapa ++
  [take c (mapa !! l) ++ [novoTerreno] ++ drop (c + 1) (mapa !! l)] ++
  drop (l + 1) mapa



