module Desenhar where

import Graphics.Gloss

import Worms
import Labs2025
import Tarefa0_2025
import Tarefa0_geral
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4



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
desenha w = -- vai ser implementada depois de todas as outras funcoes serem feitas
    Pictures [mapaPic, wormsPic, hudPic]
    where
        e = jogo w
        mapa = mapaEstado e

        alturaMapa = fromIntegral (length mapa) * tileSize

        -- funcao que determina a largura do mapa
        larguraMapa = 
            case mapa of
                [] -> 0
                (linha:_) -> fromIntegral (length linha) * tileSize

        mapaPic =
            Translate ( larguraMapa / 2 ) ( alturaMapa / 2 ) -- map width 
                (desenhaMapa mapa)

        wormsPic =
            Translate ( larguraMapa / 2 ) ( alturaMapa / 2 ) -- map width 
                (desenhaMinhocas e (selW w)) 
                
        hudPic = desenhaHUD w 

-- | funcao que desenha o mapa
desenhaMapa :: Mapa -> Picture
desenhaMapa mapa =
  Pictures
    [ Translate x y (desenhaTerreno t)
    | (linha, y) <- zip mapa [0, -tileSize ..]
    , (t, x)     <- zip linha [0, tileSize ..]
    ]


-- | Funcao que desenha as minhocas
desenhaMinhocas :: Estado -> Int -> Picture
desenhaMinhocas e sel =
  Pictures
    [ desenhaMinhoca (i == sel) pos
    | (i, m) <- zip [0..] (minhocasEstado e)
    , Just pos <- [posicaoMinhoca m]     -- só desenha se tiver posição
    , vidaMinhoca m /= Morta            -- opcional: ignora mortas
    ]

-- | Funcao que desenha o terreno 

desenhaTerreno :: Terreno -> Picture
desenhaTerreno t =
  Color (corTerreno t) (rectangleSolid tileSize tileSize)


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
        Ar          -> makeColor 0 0 0 0
        Agua        -> rgb 118 213 254 -- azul claro
        Terra       -> rgb 99 45 0 -- castanho claro
        Pedra       -> rgb 154 153 150 -- cinza escuro

-- | Funcao que desenha o HUD
desenhaHUD :: Worms -> Picture
desenhaHUD w =
  Translate (-600) 400 
    (Scale 0.15 0.15 
      (Color black 
        (Text ("Minhoca: " ++ show (selW w)))))











