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

desenhaMenu :: Worms -> Picture
desenhaMenu w =
  Pictures
    [ menuBg (assets w)  -- imagem de fundo
    , Translate (-250) 120 $ Scale 0.5 0.5 $ Color white $ Text "MENU"
    , Translate (-250)  20 $ item MenuJogar    "Jogar"    0
    , Translate (-250) (-60) $ item MenuTutorial "Tutorial" 1
    , Translate (-250) (-140) $ item MenuCreditos "Creditos" 2
    , Translate (-250) (-260) $ Scale 0.2 0.2 $ Color white $ Text "W/S ou Setas - escolher | ENTER - confirmar"
    ]
  where
    sel = menuSel w
    item mi label k =
      let col = if mi == sel then yellow else white
      in Translate 0 (fromIntegral (-80*k)) (Scale 0.35 0.35 (Color col (Text label)))

desenhaTutorial :: Worms -> Picture
desenhaTutorial w =
  Pictures
    [ menuBg (assets w)
    , Translate (-600) 200 $ Scale 0.35 0.35 $ Color white $ Text "TUTORIAL"
    , Translate (-600) 120 $ Scale 0.22 0.22 $ Color white $ Text "1 - trocar minhoca | 2 - trocar jogada"
    , Translate (-600)  60 $ Scale 0.22 0.22 $ Color white $ Text "WASD - mover"
    , Translate (-600) (-40) $ Scale 0.22 0.22 $ Color white $ Text "ESC - voltar ao menu"
    ]

desenhaCreditos :: Worms -> Picture
desenhaCreditos w =
  Pictures
    [ menuBg (assets w)
    , Translate (-600) 200 $ Scale 0.35 0.35 $ Color white $ Text "CREDITOS"
    , Translate (-600) 120 $ Scale 0.22 0.22 $ Color white $ Text "Feito por: (coloca aqui os nomes)"
    , Translate (-600)  60 $ Scale 0.22 0.22 $ Color white $ Text "UC / Ano / Turma"
    , Translate (-600) (-40) $ Scale 0.22 0.22 $ Color white $ Text "ESC - voltar ao menu"
    ]


-- | Função que desenha o estado do jogo no Gloss.
desenha :: Worms -> Picture
desenha w =
  case screen w of
    Menu     -> desenhaMenu w
    Tutorial -> desenhaTutorial w
    Creditos -> desenhaCreditos w
    EmJogo   -> Pictures [desenhaMapa mapa, desenhaMinhocas e (selW w), desenhaHUD w]
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


-- | funcao que desenha as minhocas
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

-- | funcao que desenha uma minhoca
desenhaMinhoca :: Mapa -> Bool -> Posicao -> Picture
desenhaMinhoca mapa selecionada (linha, coluna) =
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
    x = fromIntegral coluna * tileSize + tileSize / 2
    y = - (fromIntegral linha * tileSize + tileSize / 2)


-- | raio da minhoca (em pixels)
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
  Translate (-900) 480
    (Scale 0.18 0.18
      (Color black
        (Text ("Sel=" ++ show (selW w) ++ "  Pos=" ++ show pos))))
  where
    e   = jogo w
    ms  = minhocasEstado e
    pos =
      if null ms then Nothing
      else posicaoMinhoca (ms !! selW w)












