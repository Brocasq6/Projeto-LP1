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

selecionaMinhocaSeguinte :: Worms -> Worms
selecionaMinhocaSeguinte w = undefined

wormsValida :: Estado -> [Int]
wormsValida e = undefined

cycleSelW :: Estado -> Int -> Int
cycleSelW e x = undefined

selecionaJogadaSeguinte :: Worms -> Worms
selecionaJogadaSeguinte w = undefined

nextSelJogada :: SelJogada -> SelJogada
nextSelJogada j = undefined

aplicaJogadaDirecional :: Char -> Worms -> Worms
aplicaJogadaDirecional c w = undefined

dirFromKey :: Char -> Maybe Direcao
dirFromKey c = undefined

jogadaFromSel :: SelJogada -> Direcao -> Jogada
jogadaFromSel j d = undefined

aplicaEfetua :: Int -> Jogada -> Estado -> Estado
aplicaEfetua x j e = efetuaJogada x j e

wormsValidas :: Estado -> [Int]
wormsValidas e = undefined

