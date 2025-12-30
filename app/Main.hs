module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Worms
import Tempo
import Labs2025

{-
Main.hs
 │
 |- janela :: Display
 |- fundo  :: Color
 |- fr     :: Int
 |
 |- it :: Worms
 |    |- criaEstadoInicial :: Estado          (vossas tarefas / setup)
 |    |- Worms { jogo = criaEstadoInicial
 |             , selW = 0
 |            , selJ = minBound (ou default)
 |            , ... }
 |
 |- main :: IO ()
      |- play janela fundo fr it desenha reageEventos reageTempo

-}

janela :: Display
janela = InWindow "Worms" (1920, 1080) (0, 0)

fundo :: Color
fundo = black

fr :: Int
fr = 60

-- Exemplo: mapa 40x25

mapaInicial :: Mapa
mapaInicial =
  [ [ tile x y | x <- [0..w-1] ] | y <- [0..h-1] ]
  where
    w = 40
    h = 25

    tile x y
      -- “borda” em pedra
      | x == 0 || x == w-1 || y == 0 || y == h-1 = Pedra

      -- chão: últimas 5 linhas em Terra
      | y >= h-6 = Terra

      -- um “bloco” de pedra no meio (obstáculo)
      | x >= 18 && x <= 22 && y >= 12 && y <= 14 = Pedra

      -- resto é ar
      | otherwise = Ar

-- Exemplo: duas minhocas
minhoca0 :: Minhoca
minhoca0 =
  Minhoca
    { posicaoMinhoca = Just (6, 18)
    , vidaMinhoca    = Viva 100 -- ou 100 / ou outro construtor
    }

minhoca1 :: Minhoca
minhoca1 =
  Minhoca
    { posicaoMinhoca = Just (7, 18)
    , vidaMinhoca    = Viva 100
    }



-- | Função que cria o estado inicial do jogo.
estadoInicial :: Estado
estadoInicial =
  Estado
    { mapaEstado     = mapaInicial
    , minhocasEstado = [minhoca0, minhoca1]
    , objetosEstado  = []
    }

-- | Função principal que invoca o Gloss para correr o jogo.
main :: IO ()
main = do
  play janela fundo fr it desenha reageEventos reageTempo
  where
    it = defaultWorms estadoInicial
