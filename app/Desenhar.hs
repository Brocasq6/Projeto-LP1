module Desenhar where

import Graphics.Gloss

import Worms
import Labs2025




{-
    desenha
     |- desenhaMapa
     |- desenhaMinhocas
     |- desenhaHUD

-}

-- | Tamanho de cada tile no mapa (em pixels).
tileSize :: Float
tileSize = 30.0 -- valor inicial, pode ser ajustado conforme necessário

-- | Dimensões da janela do jogo
screenW, screenH :: Float
screenW = 1920
screenH = 1080



-- | Função que desenha o estado do jogo no Gloss.
desenha :: Worms -> Picture
desenha w =
  Pictures
    [ desenhaMapa mapa
    , desenhaMinhocas e (selW w)
    , desenhaHUD w
    ]
  where
    e    = jogo w
    mapa = mapaEstado e


-- | funcao que calcula o offset do mapa para o centro da janela
mapOffset :: Mapa -> (Float, Float)
mapOffset mapa = (-mapW / 2, mapH / 2)
  where
    mapH = fromIntegral (length mapa) * tileSize
    mapW =
      case mapa of
        []      -> 0
        (l : _) -> fromIntegral (length l) * tileSize



-- | funcao que desenha o mapa
desenhaMapa :: Mapa -> Picture
desenhaMapa mapa =
  Pictures
    [ Translate (offX + x) (offY + y) (desenhaTerreno t)
    | (linha, row) <- zip mapa [0..]
    , (t, col)     <- zip linha [0..]
    , let x = fromIntegral col * tileSize + tileSize/2
    , let y = - (fromIntegral row * tileSize + tileSize/2)
    ]
  where
    (offX, offY) = mapOffset mapa



desenhaMinhocas :: Estado -> Int -> Picture
desenhaMinhocas e sel =
  Pictures
    [ desenhaMinhoca mapa (i == sel) pos
    | (i, m) <- zip [0..] (minhocasEstado e)
    , Just pos <- [posicaoMinhoca m]
    , vidaMinhoca m /= Morta
    ]
  where
    mapa = mapaEstado e

desenhaMinhoca :: Mapa -> Bool -> Posicao -> Picture
desenhaMinhoca mapa selecionada (xGrid, yGrid) =
  Translate (offX + x) (offY + y)
    (Pictures
      [ -- destaque (anel) se estiver selecionada
        if selecionada
          then Color (rgb 255 215 0)
                 (circleSolid (wormRadius + 3))
          else Blank

      , -- corpo da minhoca
        Color (rgb 60 170 90)
          (circleSolid wormRadius)
      ])
  where
    -- offset para centrar o mapa
    offX = -w / 2
    offY =  h / 2

    h = fromIntegral (length mapa) * tileSize
    w =
      case mapa of
        []      -> 0
        (l : _) -> fromIntegral (length l) * tileSize

    -- posição da minhoca no ecrã (centro do tile)
    x = fromIntegral xGrid * tileSize + tileSize / 2
    y = - (fromIntegral yGrid * tileSize + tileSize / 2)

wormRadius :: Float
wormRadius = tileSize * 0.35



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
        Ar          -> rgb 255 255 255 -- branco
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











