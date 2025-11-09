module Main where

import Labs2025
import Tarefa3
import Magic

-- ==========
-- Mapas
-- ==========

mBase :: Mapa
mBase =
  [ replicate 10 Ar
  , replicate 10 Ar
  , replicate 10 Ar
  , replicate 10 Ar
  , [Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
  ]

p :: Int -> Int -> Posicao  -- (linha, coluna)
p l c = (l,c)

-- Construtores de minhocas
viva :: Posicao -> Minhoca
viva pos = Minhoca
  { posicaoMinhoca   = Just pos
  , vidaMinhoca      = Viva 100
  , jetpackMinhoca   = 3
  , escavadoraMinhoca= 3
  , bazucaMinhoca    = 3
  , minaMinhoca      = 3
  , dinamiteMinhoca  = 3
  }

mortaW :: Posicao -> Minhoca
mortaW pos = Minhoca
  { posicaoMinhoca   = Just pos
  , vidaMinhoca      = Morta
  , jetpackMinhoca   = 0
  , escavadoraMinhoca= 0
  , bazucaMinhoca    = 0
  , minaMinhoca      = 0
  , dinamiteMinhoca  = 0
  }

-- Estado helper
estado :: [Objeto] -> [Minhoca] -> Estado
estado os ws = Estado { mapaEstado = mBase, objetosEstado = os, minhocasEstado = ws }

-- Objeto helpers
barril :: Posicao -> Bool -> Objeto
barril q expl = Barril { posicaoBarril = q, explodeBarril = expl }

bazuca :: Posicao -> Direcao -> NumMinhoca -> Objeto
bazuca q d dono = Disparo { posicaoDisparo = q
                          , direcaoDisparo = d
                          , tipoDisparo = Bazuca
                          , tempoDisparo = Nothing
                          , donoDisparo = dono }

mina :: Posicao -> Direcao -> Maybe Int -> NumMinhoca -> Objeto
mina q d t dono = Disparo { posicaoDisparo = q
                          , direcaoDisparo = d
                          , tipoDisparo = Mina
                          , tempoDisparo = t
                          , donoDisparo = dono }

dinamite :: Posicao -> Direcao -> Maybe Int -> NumMinhoca -> Objeto
dinamite q d t dono = Disparo { posicaoDisparo = q
                              , direcaoDisparo = d
                              , tipoDisparo = Dinamite
                              , tempoDisparo = t
                              , donoDisparo = dono }

-- ============ Estados de teste T3 ============

-- 1) Gravidade: minhoca no ar deve cair 1 célula
e_gravidade_queda :: Estado
e_gravidade_queda =
  estado [] [viva (p 2 4)]

-- 2) Queda na água: minhoca cai e morre ao entrar em água
e_queda_morte_agua :: Estado
e_queda_morte_agua =
  estado [] [viva (p 4 8)]

-- 3) Queda para fora do mapa
e_queda_fora_mapa :: Estado
e_queda_fora_mapa = estado [] [viva (p 5 5)]

-- 4) Barril prestes a explodir
e_barril_explode :: Estado
e_barril_explode = estado [barril (p 3 6) True] [viva (p 3 4), viva (p 3 7)]

-- 5) Bazuca avança um passo para Oeste
e_bazuca_avanca :: Estado
e_bazuca_avanca = estado [bazuca (p 3 5) Oeste 0] [viva (p 3 2)]

-- 6) Bazuca sai do mapa
e_bazuca_sai_mapa :: Estado
e_bazuca_sai_mapa = estado [bazuca (p 0 0) Oeste 0] [viva (p 0 2)]

-- 7) Mina com tempo 0 explode
e_mina_t0_explode :: Estado
e_mina_t0_explode = estado [mina (p 3 6) Norte (Just 0) 1] [viva (p 3 5), viva (p 3 8)]

-- 8) Dinamite com tempo 0 explode
e_dinamite_t0_explode :: Estado
e_dinamite_t0_explode = estado [dinamite (p 3 6) Norte (Just 0) 0] [viva (p 3 4)]

-- 9) Dinamite com tempo 4 passa a 3
e_dinamite_conta :: Estado
e_dinamite_conta = estado [dinamite (p 3 6) Norte (Just 4) 0] [viva (p 3 4)]

-- 10) Dinamite no ar move em parábola
e_dinamite_parabola :: Estado
e_dinamite_parabola =
  estado [dinamite (p 3 6) Este (Just 4) 0] [viva (p 3 2)]

-- 11) Mina no ar com direção Sul cai e muda para Norte
e_mina_ar_sul_cai :: Estado
e_mina_ar_sul_cai = estado [mina (p 3 6) Sul Nothing 0] [viva (p 3 2)]

-- 12) Mina no chão imobiliza e muda para Norte
e_mina_no_chao_norte :: Estado
e_mina_no_chao_norte =
  estado [mina (p 3 4) Este Nothing 0] [viva (p 3 2)]

-- 13) Mina sem tempo ativada por minhoca próxima
e_mina_ativa_por_proximidade :: Estado
e_mina_ativa_por_proximidade =
  estado [mina (p 3 6) Norte Nothing 0] [viva (p 3 2), viva (p 3 5)]

-- 14) Barril em ar/água passa a prestes a explodir
e_barril_armado :: Estado
e_barril_armado = estado [barril (p 3 7) False] [viva (p 3 4)]

-- 15) Bazuca em superfície explode (opcional, depende da tua implementação)
e_bazuca_em_superficie_explode :: Estado
e_bazuca_em_superficie_explode = estado [bazuca (p 4 4) Este 0] [viva (p 3 2)]

-- | Lista final de testes
testesT3 :: [Estado]
testesT3 =
  [ e_gravidade_queda
  , e_queda_morte_agua
  , e_queda_fora_mapa
  , e_barril_explode
  , e_bazuca_avanca
  , e_bazuca_sai_mapa
  , e_mina_t0_explode
  , e_dinamite_t0_explode
  , e_dinamite_conta
  , e_dinamite_parabola
  , e_mina_ar_sul_cai
  , e_mina_no_chao_norte
  , e_mina_ativa_por_proximidade
  , e_barril_armado
  , e_bazuca_em_superficie_explode
  ]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
  let ins = testesT3
  outs <- mapM (runTest . avancaEstado) ins
  return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3
