module Desenhar where

import Graphics.Gloss

import Worms

{-
    desenha
     |- desenhaMapa
     |- desenhaMinhocas
     |- desenhaHUD

-}

-- | Tamanho de cada tile no mapa (em pixels).
tileSize :: Float
tileSize = undefined

-- | Função que desenha o estado do jogo no Gloss.
desenha :: Worms -> Picture
desenha _ = Translate (-450) 0 $ Scale 0.5 0.5 $ Text "Welcome to Worms!"

-- | funcao que desenha o mapa
desenhaMapa :: Mapa -> Picture
desenhaMapa = undefined

-- | Funcao que desenha as minhocas
desenhaMinhocas :: Estado -> Int -> Picture
desenhaMinhocas = undefined

-- | Funcao que desenha o terreno 
desenhaTerreno :: Terreno -> Picture
desenhaTerreno = undefined

-- | Funcao que determina a cor do terreno
corTerreno :: Terreno -> Color
corTerreno = undefined

-- | Funcao que desenha o HUD
desenhaHUD :: Worms -> Picture
desenhaHUD = undefined












