module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)

import Desenhar
import Eventos
import Worms
import Tempo
import Labs2025

janela :: Display
janela = InWindow "Worms" (1920, 1080) (0, 0)

fundo :: Color
fundo = black

fr :: Int
fr = 60

-- | Função que cria o estado inicial do jogo.
estadoInicial :: Estado
estadoInicial =
  Estado
    { mapaEstado     = mapaInicial
    , minhocasEstado = [minhoca0, minhoca1]
    , objetosEstado  = []
    }

-- | Função principal que invoca o Gloss para correr o jogo.
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)

main :: IO ()
main = do
  mBg <- loadJuicyPNG "assets/menu.png"  -- mete aqui a tua imagem (PNG)
  bg  <- case mBg of
           Nothing -> fail "Nao consegui carregar assets/menu.png"
           Just p  -> pure p

  let a  = Assets { menuBg = bg }
      it = defaultWorms a estadoInicial

  playIO janela fundo fr it
    (pure . desenha)
    (\ev st -> pure (reageEventos ev st))
    (\dt st -> pure (reageTempo dt st))

