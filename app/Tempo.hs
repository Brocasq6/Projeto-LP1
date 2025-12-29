module Tempo where

import Worms

{-
Tempo.hs
 |- type Segundos = Float
 |
 |- reageTempo :: Segundos -> Worms -> Worms
 │    |- avancaJogo :: Worms -> Worms
 │    |    |- jogo' = avancaEstado jogo         (T3)


-}

-- | Tempo em segundos.
type Segundos = Float

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> Worms
reageTempo _ it =
    let e' = avancaEstado (jogo it)
        it' = it { jogo = e' }
    in it' { selW = cycleSelW e' (selW it') }
