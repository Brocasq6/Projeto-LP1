module Main where

import Labs2025
import Tarefa4
import Magic

mBase :: Mapa
mBase =
  [ replicate 10 Ar
  , replicate 10 Ar
  , replicate 10 Ar
  , replicate 10 Ar
  , [Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
  ]

p :: Int -> Int -> Posicao
p l c = (l,c)

viva :: Posicao -> Minhoca
viva pos = Minhoca
  { posicaoMinhoca    = Just pos
  , vidaMinhoca       = Viva 100
  , jetpackMinhoca    = 3
  , escavadoraMinhoca = 3
  , bazucaMinhoca     = 3
  , minaMinhoca       = 3
  , dinamiteMinhoca   = 3
  }

morta :: Posicao -> Minhoca
morta pos = Minhoca
  { posicaoMinhoca    = Just pos
  , vidaMinhoca       = Morta
  , jetpackMinhoca    = 0
  , escavadoraMinhoca = 0
  , bazucaMinhoca     = 0
  , minaMinhoca       = 0
  , dinamiteMinhoca   = 0
  }

semPosMorta :: Minhoca
semPosMorta = Minhoca
  { posicaoMinhoca    = Nothing
  , vidaMinhoca       = Morta
  , jetpackMinhoca    = 0
  , escavadoraMinhoca = 0
  , bazucaMinhoca     = 0
  , minaMinhoca       = 0
  , dinamiteMinhoca   = 0
  }

estado :: [Objeto] -> [Minhoca] -> Estado
estado os ws = Estado { mapaEstado = mBase, objetosEstado = os, minhocasEstado = ws }

barril :: Posicao -> Bool -> Objeto
barril q expl = Barril { posicaoBarril = q, explodeBarril = expl }

bazuca :: Posicao -> Direcao -> NumMinhoca -> Objeto
bazuca q d dono = Disparo
  { posicaoDisparo = q
  , direcaoDisparo = d
  , tipoDisparo    = Bazuca
  , tempoDisparo   = Nothing
  , donoDisparo    = dono
  }

mina :: Posicao -> Direcao -> Maybe Int -> NumMinhoca -> Objeto
mina q d t dono = Disparo
  { posicaoDisparo = q
  , direcaoDisparo = d
  , tipoDisparo    = Mina
  , tempoDisparo   = t
  , donoDisparo    = dono
  }

dinamite :: Posicao -> Direcao -> Maybe Int -> NumMinhoca -> Objeto
dinamite q d t dono = Disparo
  { posicaoDisparo = q
  , direcaoDisparo = d
  , tipoDisparo    = Dinamite
  , tempoDisparo   = t
  , donoDisparo    = dono
  }

-- =====================================================
-- Testes T4 (inputs para simular o bot)
-- =====================================================

-- 1) Duelo simples no chão (dois alvos claros)
e_duelo :: Estado
e_duelo =
  estado []
    [ viva (p 3 2)   -- worm 0
    , viva (p 3 8)   -- worm 1
    ]

-- 2) Minhoca perto da água (ver se o bot faz asneira e cai)
e_pertoAgua :: Estado
e_pertoAgua =
  estado []
    [ viva (p 3 7)   -- acima de Ar, mas “perto” da zona de água
    , viva (p 3 4)
    ]

-- 3) Já existe bazuca ativa do dono 0 (testa restrição de “não ter 2 disparos iguais”)
e_comBazucaAtiva :: Estado
e_comBazucaAtiva =
  estado [ bazuca (p 3 5) Oeste 0 ]
    [ viva (p 3 2)
    , viva (p 3 8)
    ]

-- 4) Mina sem tempo e inimigo perto (deverá ativar por proximidade na vossa T3)
e_mina_proximidade :: Estado
e_mina_proximidade =
  estado [ mina (p 3 6) Norte Nothing 0 ]
    [ viva (p 3 2)   -- dono
    , viva (p 3 5)   -- inimigo perto da mina
    ]

-- 5) Estado “degenerado”: todas mortas/sem posição (o bot não pode crashar)
e_todasMortas :: Estado
e_todasMortas =
  estado []
    [ morta (p 3 2)
    , semPosMorta
    ]

-- | Definir aqui os testes do grupo para a Tarefa 4
testesTarefa4 :: [Estado]
testesTarefa4 =
  [ e_duelo
  , e_pertoAgua
  , e_comBazucaAtiva
  , e_mina_proximidade
  , e_todasMortas
  ]

dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4