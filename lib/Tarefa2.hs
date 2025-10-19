{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025

data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)


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

moveMinhoca :: Direcao -> Mapa -> Minhoca -> Minhoca
moveMinhoca dir mapa minhoca = undefined

movePosicao :: Direcao -> Posicao -> Posicao
movePosicao direcao (x,y) = 
    case direcao of
        Norte -> (x, y-1)
        Nordeste -> (x-1, y+1)
        Este -> (x, y+1)
        Sudeste -> (x+1, y+1)
        Sul -> (x+1, y)
        Sudoeste -> (x+1, y-1)
        Oeste -> (x, y-1)
        Noroeste -> (x-1, y-1)

terrenoNaPosicao :: Mapa -> Posicao -> Terreno
terrenoNaPosicao mapa (x,y) = (mapa !! y) !! x

aplicaEfeitoTerreno :: Minhoca -> Posicao -> Terreno -> Minhoca
aplicaEfeitoTerreno minhoca pos terreno = undefined

efetuaJogadaMove :: NumMinhoca -> Direcao -> Estado -> Estado
efetuaJogadaMove n dir est = undefined

--------------------------------------- funcoes relacionadas com a funcao efetuaJogadaDisparo -------------------------------------------------

temMunicao :: TipoArma -> Bool
temMunicao arma mun = 
    case arma of 
        Jetpack -> jetpackMinhoca municao > 0 
        Escavadora -> escavadoraMinhoca municao > 0 
        Bazuca -> bazucaMinhoca municao > 0 
        Mina -> minaMinhoca municao > 0 
        Dinamite -> dinamiteMinhoca municao > 0 

consomeMunicao :: TipoArma -> Minhoca -> Minhoca
consomeMunicao arma municao = 
    case arma of
        Jetpack -> municao { jetpackMinhoca = jetpackMinhoca municao - 1 }
        Escavadora -> municao { escavadoraMinhoca = escavadoraMinhoca municao - 1 }
        Bazuca -> municao { bazucaMinhoca = bazucaMinhoca municao - 1 }
        Mina -> municao { minaMinhoca = minaMinhoca municao - 1 }
        Dinamite -> municao {dinamiteMinhoca = dinamiteMinhoca municao - 1 }

criaDisparo :: TipoArma -> Direcao -> NumMinhoca -> Minhoca -> Objeto
criaDisparo arma dir dono minhoca = 

atualizaLista :: Int -> a -> [a] -> [a]
atualizaLista i novo l = take i l ++ [novo] ++ drop (i + 1) l 

efetuaJogadaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
efetuaJogadaDisparo n arma dir estado = undefined 

----------------------------------------------------------------------------------------------------------------

-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n jogada estado =
  case jogada of
    Move dir -> efetuaJogadaMove n dir estado
    Dispara arma dir -> efetuaJogadaDispara n arma dir estado

