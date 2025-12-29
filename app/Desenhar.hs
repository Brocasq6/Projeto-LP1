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
tileSize = 20.0 -- valor inicial, pode ser ajustado conforme necessário

-- | Função que desenha o estado do jogo no Gloss.
desenha :: Worms -> Picture
desenha w = undefined -- vai ser implementada depois de todas as outras funcoes serem feitas


-- | funcao que desenha o mapa
desenhaMapa :: Mapa -> Picture
desenhaMapa = undefined

-- | Funcao que desenha as minhocas
desenhaMinhocas :: Estado -> Int -> Picture
desenhaMinhocas = undefined

-- | Funcao que desenha o terreno 

desenhaTerreno :: Terreno -> Picture
desenhaTerreno t =
  Color (corTerreno t) $
    rectangleSolid tileSize tileSize


-- | Funcao que converte valores RGB para o formato Color do Gloss (auxiliar para corTerreno)
rgb :: Int -> Int -> Int -> Color
rgb r g b =
  makeColor
    (fromIntegral r / 255)
    (fromIntegral g / 255)
    (fromIntegral b / 255)
    1

-- | Funcao que determina a cor do terreno
corTerreno :: Terreno -> Color
corTerreno t = 
    case t of
        Agua    -> undefined
        Ar      -> undefined
        Terra   -> undefined
        Pedra   -> undefined

-- | Funcao que desenha o HUD
desenhaHUD :: Worms -> Picture
desenhaHUD = undefined












