{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Tarefa3 where 
import Labs2025
import Tarefa1 hiding (terrenoNaPosicao) 
 


{-

avancaEstado
 |
 |── avancaMinhoca
 |     |── posicaoMinhoca
 |     |── terrenoNaPosicao
 |     |── estaNoArOuAgua
 |     |── posicaoValida
 |     └── aplicaGravidade
 |
 |── avancaObjeto
 |     |── tipoObjeto
 |     |     |── Barril
 |     |     |    |── verificaExplosao
 |     |     |    |── estaNoArOuAgua
 |     |     |
 |     |     |── Disparo
 |     |           |── moveDisparo
 |     |           |── verificaColisao
 |     |           |── contaTempo
 |     |           |── ativaMina
 |     |           └── geraExplosao
 |     |
 |     └── criaListaDanos
 |
 └── aplicaDanos
       |── calculaDanoMinhocas
       |     |── verificaPosicaoAfetada
       |     └── reduzVidaOuMata
       |
       |── atualizaMapa
       |     |── removeTerrenoAtingido
       |
       └── atualizaObjetos
             |── removeExplodidos
             |── propagaExplosao

-}


-- | Tipo de dado para representar danos em posições.
type Dano = Int
-- | Tipo de dado para representar uma lista de danos em várias posições.
type Danos = [(Posicao,Dano)]

-- | Função principal da Tarefa 3. Avanço o estado do jogo um tick.
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado _ objetos minhocas) =
  let
    -- 1) atualizar minhocas
    minhocas' =
      map (uncurry (avancaMinhoca e)) (zip [0..] minhocas)

    -- 2) avançar objetos com as novas minhocas já no estado
    eComMinhocas = e { minhocasEstado = minhocas' }

    (objetos', danosLists) =
      unzip (map (uncurry (avancaObjeto eComMinhocas)) (zip [0..] objetos))

    -- 3) aplicar todos os danos
    danos = concat danosLists
    eFinal = eComMinhocas {objetosEstado = objetos'}
  in
    aplicaDanos danos eFinal

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
avancaObjeto :: Estado -> NumObjeto -> Objeto -> (Objeto, Danos)
avancaObjeto estado _ objeto =
  case tipoObjeto objeto of
    OBarril -> avancaBarril  estado objeto
    ODisparo -> avancaDisparo estado objeto

-- | move APENAS a minhoca idx segundo as regras dos testes
avancaBarril :: Estado -> Objeto -> (Objeto, Danos)
avancaBarril estado (Barril posicao explodir)
  | explodir = (Barril (-1,-1) False, geraExplosao posicao 5)
  | estaNoArOuAgua posicao (mapaEstado estado) = (Barril posicao True, [])
  | otherwise = (Barril posicao explodir, [])
avancaBarril _ objeto = (objeto, [])

-- | move APENAS a minhoca idx segundo as regras dos testes
avancaDisparo :: Estado -> Objeto -> (Objeto, Danos)
avancaDisparo estado objeto =
  case tipoDisparo objeto of
    Bazuca -> avancaBazuca   estado objeto
    Mina -> avancaMina     estado objeto
    Dinamite -> avancaDinamite estado objeto
    Jetpack -> (objeto, [])
    Escavadora -> (objeto, [])

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
avancaBazuca :: Estado -> Objeto -> (Objeto, Danos)
avancaBazuca estado objeto =
  let mapa     = mapaEstado estado
      -- opcional: não considerar o próprio disparo na lista de colisões
      objs     = filter (/= objeto) (objetosEstado estado)
      pos0     = posicaoDisparo objeto
      nova     = moveDisparo (direcaoDisparo objeto) pos0
      removido = objeto { posicaoDisparo = (-1,-1) }
  in case colisaoBazuca nova mapa objs of
       Fora  -> (removido, [])
       Bate  -> (removido, geraExplosao nova 5)
       Livre -> (objeto { posicaoDisparo = nova }, [])

-- | Avança o estado de uma Mina.
avancaMina :: Estado -> Objeto -> (Objeto, Danos)
avancaMina estado objeto =
  case tempoDisparo objeto of
    Just 0 -> (objeto {posicaoDisparo = (-1,-1)}, geraExplosao (posicaoObjeto objeto) 3)
    _ ->
      let ativada   = ativaMina estado objeto
          novaPos   = aplicaGravidade (posicaoObjeto ativada) (mapaEstado estado)
          novoTempo = contaTempo ativada
      in case novoTempo of
           Just 0 -> (ativada {posicaoDisparo = (-1,-1)}, geraExplosao novaPos 3)
           _      -> (ativada {posicaoDisparo = novaPos , tempoDisparo = novoTempo} , [])

-- | move APENAS a minhoca idx segundo as regras dos testes
avancaDinamite :: Estado -> Objeto -> (Objeto, Danos)
avancaDinamite estado objeto =
  case tempoDisparo objeto of
    Just 0 -> (objeto { posicaoDisparo = (-1,-1) }, geraExplosao (posicaoObjeto objeto) 7)
    _ ->
      let novaPos = aplicaGravidade (posicaoObjeto objeto) (mapaEstado estado)
          novoTempo = contaTempo objeto
      in case novoTempo of
           Just 0 -> (objeto { posicaoDisparo = (-1,-1) }, geraExplosao novaPos 7)
           _ -> (objeto { posicaoDisparo = novaPos, tempoDisparo = novoTempo }, [])

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
verificaColisao pos mapa objs =
  let parede = case terrenoNaPosicao pos mapa of
                 Nothing   -> True
                 Just Pedra -> True
                 Just Terra -> True
                 _          -> False
      ocupadoPorObj = any (\o -> posicaoObjeto o == pos) objs
  in parede || ocupadoPorObj

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
data TipoObjeto = OBarril | ODisparo
  deriving (Eq, Show)

-- | retorna o tipo de objeto
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


