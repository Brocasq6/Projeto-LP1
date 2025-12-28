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

-- | Função que avança para a próxima minhoca selecionada.
selecionaMinhocaSeguinte :: Worms -> Worms
selecionaMinhocaSeguinte w = 
    let e = jogo w
        x = selW w
        novaSel = cycleSelW e x
    in w { selW = novaSel }

-- | Função que retorna as minhocas válidas no estado do jogo.
wormsValida :: Estado -> [Int]
wormsValida e = undefined

-- | Função que avança para a próxima minhoca selecionada.
cycleSelW :: Estado -> Int -> Int
cycleSelW e x = undefined

-- | Função que avança para a próxima minhoca selecionada.
selecionaJogadaSeguinte :: Worms -> Worms
selecionaJogadaSeguinte w = undefined

-- | Função que avança para a próxima jogada selecionada.
nextSelJogada :: SelJogada -> SelJogada
nextSelJogada j = undefined

-- | Função que aplica uma jogada direcional ao estado do jogo.
aplicaJogadaDirecional :: Char -> Worms -> Worms
aplicaJogadaDirecional c w = undefined

-- | Função que converte uma tecla em direção.
dirFromKey :: Char -> Maybe Direcao
dirFromKey c = undefined

-- | Função que converte uma seleção de jogada e direção em jogada.
jogadaFromSel :: SelJogada -> Direcao -> Jogada
jogadaFromSel j d = undefined

-- | Função que aplica uma jogada ao estado do jogo.
aplicaEfetua :: Int -> Jogada -> Estado -> Estado
aplicaEfetua x j e = efetuaJogada x j e

-- | Função que retorna as minhocas válidas no estado do jogo.
wormsValidas :: Estado -> [Int]
wormsValidas e = undefined

