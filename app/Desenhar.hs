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
    [ Translate (offX + x) (offY + y) (desenhaTerreno t)
    | (linha, row) <- zip mapa [0..]
    , (t, col)     <- zip linha [0..]
    , let x = fromIntegral col * tileSize + tileSize/2
    , let y = - (fromIntegral row * tileSize + tileSize/2)
    ]
  where
    h = fromIntegral (length mapa) * tileSize
    w =
      case mapa of
        []      -> 0
        (l : _) -> fromIntegral (length l) * tileSize

    offX = -w / 2
    offY =  h / 2


desenhaMinhoca :: Bool -> Posicao -> Picture
desenhaMinhoca selecionada (xGrid, yGrid) =
  Translate x y
    (Pictures
      [ -- destaque se estiver selecionada
        if selecionada
          then Color (rgb 255 215 0)
                 (circleSolid (wormRadius + 3))
          else Blank

      , -- corpo da minhoca
        Color (rgb 60 170 90)
          (circleSolid wormRadius)

      , -- olho simples (opcional)
        Translate (wormRadius / 3) (wormRadius / 4)
          (Color black (circleSolid 2))
      ])
  where
    x = fromIntegral xGrid * tileSize
    y = - fromIntegral yGrid * tileSize

wormRadius :: Float
wormRadius = tileSize * 0.35

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











