{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025

{-

efetuaJogada
  ── efetuaJogadaMove
 │      ── moveMinhoca
 │     │      ── proximaPosicao
 │     │      ── terrenoNaPosicao
 │     │      ── aplicaEfeitoTerreno
 │      ── atualizaLista
 │
  ── efetuaJogadaDispara
       ── temMunicao
       ── consomeMunicao
       ── criaDisparo
       ── atualizaLista

-}


-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n jogada estado =
  case jogada of
    Move dir -> efetuaJogadaMove n dir estado
    Dispara arma dir -> efetuaJogadaDispara n arma dir estado

