module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Worms
import Tempo

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
fundo = white

fr :: Int
fr = 60

-- | Função principal que invoca o Gloss para correr o jogo.
main :: IO ()
main = do
  putStrLn "Hello from Worms!"

  play janela fundo fr it desenha reageEventos reageTempo
  where
    it = Worms {}
