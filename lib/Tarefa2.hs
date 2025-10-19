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

moveMinhoca :: Direcao -> Mapa -> Minhoca -> Minhoca
moveMinhoca dir mapa minhoca = undefined

proximaPosicao :: Direcao -> Posicao -> Posicao
proximaPosicao dir (x,y) = undefined

terrenoNaPosicao :: Mapa -> Posicao -> Terreno
terrenoNaPosicao = undefined

aplicaEfeitoTerreno :: Minhoca -> Posicao -> Terreno -> Minhoca
aplicaEfeitoTerreno minhoca pos terreno = undefined

efetuaJogadaMove :: NumMinhoca -> Direcao -> Estado -> Estado
efetuaJogadaMove n dir est = undefined

--------------------------------------- funcoes relacionadas com a funcao efetuaJogadaDisparo -------------------------------------------------

temMunicao :: TipoArma -> Bool
temMunicao arma mun = undefined

consomeMunicao :: TipoArma -> Minhoca -> Minhoca
consomeMunicao arma minhoca = undefined

criaDisparo :: TipoArma -> Direcao -> NumMinhoca -> Minhoca -> Objeto
criaDisparo arma dir dono minhoca = undefined

atualizaLista :: Int -> a -> [a] -> [a]
atualizaLista i novo l = undefined 

efetuaJogadaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
efetuaJogadaDisparo n arma dir estado = undefined 

----------------------------------------------------------------------------------------------------------------

-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n jogada estado =
  case jogada of
    Move dir -> efetuaJogadaMove n dir estado
    Dispara arma dir -> efetuaJogadaDispara n arma dir estado

