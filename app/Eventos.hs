module Eventos where

import Graphics.Gloss.Interface.Pure.Game

import Worms
{-
Eventos.hs
 |- reageEventos :: Event -> Worms -> Worms
 │    |- (tecla '1')  -> selecionaMinhocaSeguinte
 │    |- (tecla '2')  -> selecionaJogadaSeguinte
 │    |- (W/A/S/D)    -> aplicaJogadaDirecional
 │    |- (outras)     -> mantém estado
 │
 |- selecionaMinhocaSeguinte :: Worms -> Worms
 |    |- wormsValidas :: Estado -> [Int]
 |    |- cycleSelW    :: Estado -> Int -> Int
 |
 |- selecionaJogadaSeguinte :: Worms -> Worms
 |    |- nextSelJogada :: SelJogada -> SelJogada
 |
 |- aplicaJogadaDirecional :: Char -> Worms -> Worms
 |    |- dirFromKey     :: Char -> Maybe Direcao
 |    |- jogadaFromSel  :: SelJogada -> Direcao -> Jogada
 |    |- aplicaEfetua   :: Int -> Jogada -> Estado -> Estado
 |         |- efetuaJogada (T2)
 |
 |- dirFromKey :: Char -> Maybe Direcao
 |- jogadaFromSel :: SelJogada -> Direcao -> Jogada
 |- wormsValidas :: Estado -> [Int]

-}

-- | Função que altera o estado do jogo no Gloss.
reageEventos :: Event -> Worms -> Worms
reageEventos _ it = it
