{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Tarefa3 where

import Data.Either

import Labs2025
import Tarefa0_2025
import Tarefa0_geral
import Tarefa1 
import Tarefa2    
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
      case posicaoMinhoca mihoca of
        Nothing -> mihoca
        Just posicao -> atualizaPosicaoGravidade estado minhoca posicao

-- | Atualiza a posição de uma minhoca aplicando a gravidade.
atualizaPosicaoGravidade :: Estado -> Minhoca -> Posicao -> Minhoca
atualizaPosicaoGravidade estado minhoca posicao =
    | not (dentroMapa abaixo mapa) = mataMinhoca minhoca Nothing -- se a posicao abaixo nao estiver dentro do mapa, a minhoca morre
    | estaNoArOuAgua abaixo mapa =
        case terrenoNaPosicao abaixo mapa of
            Just Agua -> minhoca { posicaoMinhoca = Just (aplicaGravidade abaixo mapa) } -- se estiver na agua, aplica a gravidade normalmente
            Just Ar -> minhoca { posicaoMinhoca = Just (aplicaGravidade abaixo mapa) } -- se estiver no ar, aplica a gravidade normalmente  
    | otherwise = minhoca -- se ja se encontrar no chao, nao faz nada
    where
        mapa = mapaEstado estado
        abaixo = (fst posicao + 1, snd posicao)

-- | verifica se uma mihoca está morta
minhocaMorta :: Minhoca -> Bool
minhocaMorta minhoca =
    case vidaMinhoca minhoca of
       Morta -> True
       Viva _ -> False

mataMinhoca :: Minhoca -> Minhoca
mataMinhoca minhoca posicao = minhoca {vidaMinhoca = Morta, posicaoMinhoca = posicao}

posicaoMinhoca :: Minhoca -> Maybe Posicao
posicaoMinhoca minhoca = 
    case minhoca of
        Minhoca { posicaoMinhoca = posicao } -> posicao 

terrenoNaPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoNaPosicao (linha,coluna) minhoca
  | dentroMapa (linha,coluna) minhoca = Just ((minhoca !! linha) !! coluna)
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
avancaObjeto estado indice objeto
    case tipoObjeto objeto of
        Barril -> avancaBarril estado objeto
        Disparo -> avancaDisparo estado objeto

avancaBarril :: Estado -> Objeto -> Either Objeto Danos
avancaBarril estado objeto = 
    | explodeBarril objeto = (removerObjeto, geraExplosao (posicaoObjeto objeto) 5)
    | estaNoArOuAgua (posicaoObjeto objeto) (mapaEstado estado) = (0 { explodeBarril = True }, [])
    | otherwise = (objeto, [])
    where
        removerObjeto = objeto { explodeBarril = True }


avancaDisparo :: Estado -> Objeto -> Either Objeto Danos
avancaDisparo estado objeto = 
    case tipoDisparo objeto of
        Bazuca -> avancaBazuca estado objeto 
        Mina -> avancaMina estado objeto
        Dinamite -> avancaDinamite estado objeto
        Jetpack -> (objeto, [])
        Escavadora -> (objeto, [])

avancaBazuca :: Estado -> Objeto -> Either Objeto Danos 
avancaBazuca estado objeto = undefined

avancaMina :: Estado -> Objeto -> Either Objeto Danos
avancaMina estado objeto = undefined

avancaDinamite :: Estado -> Objeto -> Either Objeto Danos
avancaDinamite estado objeto = undefined


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

verificaColisao :: Posicao -> Mapa -> Bool
verificaColisao posicao mapa =
    case terrenoNaPosicao posicao mapa of
        Nothing -> True
        Just Pedra -> True
        Just Terra -> True
        _ -> False

contaTempo :: Objeto -> Objeto
contaTempo objeto =
    case tipoDisparo objeto of
        Nothing -> Nothing
        Just t  -> Just (t-1)

ativaMina :: Objeto -> Objeto
ativaMina objeto = undefined

geraExplosao :: Posicao -> Dano -> Danos
geraExplosao posicao dano = undefined   


type Dano = Int
type Danos = [(Posicao,Dano)]

criaListaDanos :: Posicao -> Dano -> Danos
criaListaDanos posicao dano = 

data TipoObjeto = OBarril | ODisparo
  deriving (Eq, Show)

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
aplicaDanos ds e = undefined
