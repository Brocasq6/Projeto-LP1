module Main where

import Labs2025
import Tarefa1
import Magic

{-# OPTIONS_GHC -Wno-unused-binds #-}
module Testes where

import Labs2025

-- =====================================================
-- MAPAS AUXILIARES
-- =====================================================

mapaValido :: Mapa
mapaValido =
  [ [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Ar,Ar]
  ]

mapaInvalidoVazio :: Mapa
mInvalidoVazio = []

mapaInvalidoIrregular :: Mapa
mInvalidoIrregular = [[Ar,Ar,Ar],[Ar,Ar],[Ar]]

mapaAgua :: Mapa
mapaAgua =
  [ [Agua,Agua,Agua]
  , [Agua,Agua,Agua]
  , [Agua,Agua,Agua]
  ]

mapaSoAr :: Mapa
mapaSoAr = replicate 5 (replicate 5 Ar)

mamaMinimo :: Mapa
mapaMinimo = [[Ar]]

-- =====================================================
-- OBJETOS E MINHOCAS AUXILIARES
-- =====================================================

minhocaViva :: Posicao -> Minhoca
minhocaViva p =
  Minhoca { posicaoMinhoca = Just p, vidaMinhoca = Viva 100
          , jetpackMinhoca = 1, escavadoraMinhoca = 1
          , bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1 }

minhocaMorta :: Posicao -> Minhoca
minhocaMorta p =
  Minhoca { posicaoMinhoca = Just p, vidaMinhoca = Morta
          , jetpackMinhoca = 0, escavadoraMinhoca = 0
          , bazucaMinhoca = 0, minaMinhoca = 0, dinamiteMinhoca = 0 }

minhocaSemPosMorta :: Minhoca
minhocaSemPosMorta =
  Minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta
          , jetpackMinhoca = 0, escavadoraMinhoca = 0
          , bazucaMinhoca = 0, minaMinhoca = 0, dinamiteMinhoca = 0 }

-- =====================================================
-- ESTADOS DE TESTE
-- =====================================================

-- Válidos
eValido1  = Estado mValido [] [minhocaViva (3,4)]
eValido2  = Estado mValido [Barril (3,0) False] [minhocaViva (3,4), minhocaViva (2,0)]
eValido3  = Estado mMinimo [] [minhocaMorta (0,0)]
eValido4  = Estado mSoAr [] [minhocaViva (0,0)]
eValido5  = Estado mValido [Disparo (2,2) Este Bazuca Nothing 0] [minhocaViva (3,4)]
eValido6  = Estado mAgua [] [minhocaMorta (1,1)]
eValido7  = Estado mValido [] [minhocaSemPosMorta]
eValido8  = Estado mValido [Disparo (3,3) Este Bazuca Nothing 0] [minhocaViva (3,4)]
eValido9  = Estado mValido [Disparo (3,3) Este Dinamite (Just 3) 0] [minhocaViva (3,4)]
eValido10 = Estado mValido [Disparo (3,3) Este Mina (Just 2) 0] [minhocaViva (3,4)]
eValido11 = Estado mValido [Barril (4,4) False] [minhocaViva (3,4)]
eValido12 = Estado mValido [] [minhocaViva (3,4), minhocaViva (3,5)]
eValido13 = Estado mValido [Barril (2,2) False, Disparo (3,3) Este Bazuca Nothing 0] [minhocaViva (3,4)]
eValido14 = Estado mValido [] [Minhoca (Just (3,4)) (Viva 100) 0 0 0 0 0]
eValido15 = Estado mValido [] [Minhoca (Just (3,4)) (Viva 0) 0 0 0 0 0]
eValido16 = Estado mValido [] [minhocaViva (3,4), minhocaViva (2,1)]
eValido17 = Estado mValido [] [minhocaViva (3,4)]
eValido18 = Estado mValido [Disparo (3,3) Este Mina Nothing 0] [minhocaViva (3,4)]
eValido19 = Estado mValido [] [minhocaViva (0,4)]
eValido20 = Estado (replicate 3 (replicate 8 Terra)) [] [minhocaViva (1,4)]
eValido21 = Estado (replicate 8 (replicate 3 Terra)) [] [minhocaViva (3,1)]
eValido22 = Estado mValido [Barril (2,2) False, Disparo (3,4) Este Bazuca Nothing 0] [minhocaViva (3,5)]
eValido23 = Estado mValido [Disparo (3,3) Este Bazuca Nothing 0] [minhocaViva (3,4), minhocaMorta (2,2)]
eValido24 = Estado (replicate 10 (replicate 10 Ar)) [] [minhocaViva (5,5)]
eValido25 = Estado mValido [] [minhocaViva (3,1), minhocaViva (3,8)]

-- Inválidos
eInvalido1  = Estado mInvalidoVazio [] [minhocaViva (0,0)]
eInvalido2  = Estado mInvalidoIrregular [] [minhocaViva (0,0)]
eInvalido3  = Estado mValido [] [minhocaViva (10,10)]
eInvalido4  = Estado mAgua [] [minhocaViva (1,1)]
eInvalido5  = Estado mValido [] [Minhoca Nothing (Viva 50) 1 1 1 1 1]
eInvalido6  = Estado mValido [] [Minhoca (Just (3,4)) (Viva 120) 1 1 1 1 1]
eInvalido7  = Estado mValido [] [Minhoca (Just (3,4)) (Viva 100) (-1) 0 0 0 0]
eInvalido8  = Estado mValido [Barril (3,4) False] [minhocaViva (3,4)]
eInvalido9  = Estado mValido [] [minhocaViva (3,4), minhocaViva (3,4)]
eInvalido10 = Estado mValido [Disparo (3,3) Este Bazuca (Just 1) 0] [minhocaViva (3,4)]
eInvalido11 = Estado mValido [Disparo (3,3) Este Mina (Just 3) 0] [minhocaViva (3,4)]
eInvalido12 = Estado mValido [Disparo (3,3) Este Dinamite (Just 5) 0] [minhocaViva (3,4)]
eInvalido13 = Estado mValido [Disparo (3,3) Este Jetpack Nothing 0] [minhocaViva (3,4)]
eInvalido14 = Estado mValido [Disparo (3,3) Este Escavadora Nothing 0] [minhocaViva (3,4)]
eInvalido15 = Estado mValido [Disparo (3,3) Este Bazuca Nothing 9] [minhocaViva (3,4)]
eInvalido16 = Estado mValido [Disparo (3,3) Este Bazuca Nothing 0, Disparo (3,2) Este Bazuca Nothing 0] [minhocaViva (3,4)]
eInvalido17 = Estado mValido [Barril (9,9) False] [minhocaViva (3,4)]
eInvalido18 = Estado mValido [Barril (5,1) False] [minhocaViva (3,4)]
eInvalido19 = Estado mValido [Barril (2,2) False, Disparo (2,2) Este Mina Nothing 0] [minhocaViva (3,4)]
eInvalido20 = Estado mValido [Barril (2,2) False, Disparo (2,2) Este Dinamite (Just 4) 0] [minhocaViva (3,4)]
eInvalido21 = Estado mValido [Disparo (3,3) Este Bazuca (Just (-1)) 0] [minhocaViva (3,4)]
eInvalido22 = Estado mValido [] [minhocaViva (3,4)]  -- terreno desconhecido não representável
eInvalido23 = Estado mValido [] [Minhoca (Just (3,4)) (Viva 100) 1 1 1 1 1] -- placeholder
eInvalido24 = Estado mValido [] []
eInvalido25 = Estado mValido [Barril (3,4) False] [minhocaViva (3,4)]
eInvalido26 = Estado mValido [Disparo (3,3) Este Bazuca Nothing 0, Disparo (2,2) Este Bazuca Nothing 0] [minhocaViva (3,4)]
eInvalido27 = Estado mValido [Disparo (9,9) Este Bazuca Nothing 0] [minhocaViva (3,4)]
eInvalido28 = Estado [[]] [] [minhocaViva (0,0)]
eInvalido29 = Estado mValido [Barril (3,3) False, Barril (3,3) False] [minhocaViva (3,4)]

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 =
  [ eValido1, eValido2, eInvalido1, eInvalido2, eInvalido3, eInvalido4
  , eInvalido5, eInvalido6, eInvalido7, eInvalido8, eInvalido9, eInvalido10
  , eInvalido11, eInvalido12, eInvalido13, eInvalido14, eInvalido15, eInvalido16
  , eInvalido17, eInvalido18, eInvalido19, eInvalido20, eValido3, eValido4
  , eValido5, eValido6, eValido7, eValido8, eValido9, eValido10, eValido11
  , eValido12, eValido13, eValido14, eValido15, eInvalido21, eInvalido22
  , eInvalido23, eInvalido24, eInvalido25, eInvalido26, eInvalido27, eInvalido28
  , eInvalido29, eValido16, eValido17, eValido18, eValido19, eValido20
  , eValido21, eValido22, eValido23, eValido24, eValido25
  ]



    
dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1
