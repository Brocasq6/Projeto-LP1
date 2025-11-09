module Main where

import Labs2025
import Tarefa2
import Magic 

-- ─────────────────────────────
-- Mapas simples de apoio
-- ─────────────────────────────
mBase :: Mapa
mBase =
  [ replicate 10 Ar
  , replicate 10 Ar
  , replicate 10 Ar
  , replicate 10 Ar
  , [Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
  ]

-- Posições convenientes (colunas 0..9, linhas 0..5)
p x y = (x,y)  -- (linha, coluna)

-- Minhocas "completas"
vivaFull :: Posicao -> Minhoca
vivaFull pos =
  Minhoca { posicaoMinhoca = Just pos
          , vidaMinhoca = Viva 100
          , jetpackMinhoca = 3
          , escavadoraMinhoca = 3
          , bazucaMinhoca = 3
          , minaMinhoca = 3
          , dinamiteMinhoca = 3
          }

viva1, viva2 :: Posicao -> Minhoca
viva1 = vivaFull
viva2 = vivaFull

-- Minhoca sem munições (para testar falhas de disparo)
semMuns :: Posicao -> Minhoca
semMuns pos =
  Minhoca { posicaoMinhoca = Just pos
          , vidaMinhoca = Viva 100
          , jetpackMinhoca = 0
          , escavadoraMinhoca = 0
          , bazucaMinhoca = 0
          , minaMinhoca = 0
          , dinamiteMinhoca = 0
          }

-- Minhoca morta (para validar regras)
morta :: Posicao -> Minhoca
morta pos =
  Minhoca { posicaoMinhoca = Just pos
          , vidaMinhoca = Morta
          , jetpackMinhoca = 1
          , escavadoraMinhoca = 1
          , bazucaMinhoca = 1
          , minaMinhoca = 1
          , dinamiteMinhoca = 1
          }

-- Estado "limpo": sem objetos
estado0 :: [Minhoca] -> Estado
estado0 ws = Estado { mapaEstado = mBase, objetosEstado = [], minhocasEstado = ws }

-- Estado com um barril numa borda do chão
estadoBarril :: [Minhoca] -> Estado
estadoBarril ws =
  Estado { mapaEstado = mBase
         , objetosEstado = [ Barril { posicaoBarril = p 4 0, explodeBarril = False } ]
         , minhocasEstado = ws
         }

-- Estado com uma bazuca já em voo do dono 0 (para testar restrição de disparos simultâneos)
estadoComBazucaDo0 :: [Minhoca] -> Estado
estadoComBazucaDo0 ws =
  Estado { mapaEstado = mBase
         , objetosEstado = [ Disparo { posicaoDisparo = p 3 3
                                     , direcaoDisparo = Oeste
                                     , tipoDisparo = Bazuca
                                     , tempoDisparo = Nothing
                                     , donoDisparo = 0 } ]
         , minhocasEstado = ws
         }

-- Estado com uma mina do dono 1
estadoComMinaDo1 :: [Minhoca] -> Estado
estadoComMinaDo1 ws =
  Estado { mapaEstado = mBase
         , objetosEstado = [ Disparo { posicaoDisparo = p 4 6
                                     , direcaoDisparo = Sul
                                     , tipoDisparo = Mina
                                     , tempoDisparo = Nothing
                                     , donoDisparo = 1 } ]
         , minhocasEstado = ws
         }

-- Construções frequentes de pares de minhocas
wA  = viva1 (p 4 5)
wAChao = viva1 (p 3 5)
wChao = viva1 (p 3 4)
wTopo = viva1 (p 0 0)
wDireitaChao = viva1 (p 3 3)
wAgua = viva1 (p 5 8)
wB = viva2 (p 3 7)

-- Estados base utilizados nos testes
eChao      = estado0 [wChao, wB]
eAr        = estado0 [wA, wB]
eTopo      = estado0 [wTopo, wB]
eAgua      = estado0 [wAgua, wB]
eSemMuns   = estado0 [semMuns (p 3 4), wB]
eMorta     = estado0 [morta (p 3 4), wB]
eComBarril = estadoBarril [wChao, wB]
eBazuca0   = estadoComBazucaDo0 [wChao, wB]
eMina1     = estadoComMinaDo1 [wChao, wB]

-- Para alguns casos, colocamos a minhoca 0 num canto para testar limites
eLimiteOeste = estado0 [viva1 (p 3 0), wB]
eLimiteNorte = estado0 [viva1 (p 0 5), wB]
eLimiteSul   = estado0 [viva1 (p 5 2), wB]
eLimiteLeste = estado0 [viva1 (p 3 9), wB]

-- Jogadas curtas convenientes
mvN  = Move Norte
mvNE = Move Nordeste
mvE  = Move Este
mvS  = Move Sul
mvO  = Move Oeste

jpN  = Dispara Jetpack Norte
escE = Dispara Escavadora Este
bzE  = Dispara Bazuca Este
mnS  = Dispara Mina Sul
dnO  = Dispara Dinamite Oeste

-- | Lista de testes para Tarefa 2
-- Cada tuplo: (indice da minhoca, jogada, estado inicial)
testesT2 :: [(NumMinhoca, Jogada, Estado)]
testesT2 =
  [ (0, mvNE, eChao)
  , (0, mvN , eChao)
  , (0, mvE , eChao)
  , (0, mvN , eAr)
  , (0, mvS , eTopo)
  , (0, mvO , eLimiteOeste)
  , (0, mvN , eLimiteNorte)
  , (0, mvE , eLimiteLeste)
  , (0, mvS , eLimiteSul)
  , (0, mvE , eAgua)
  , (0, jpN , eChao)
  , (0, escE, eChao)
  , (0, bzE , eChao)
  , (0, mnS , eChao)
  , (0, dnO , eChao)
  , (0, bzE , eBazuca0)
  , (1, mnS , eMina1)
  , (0, bzE , eSemMuns)
  , (0, jpN , eMorta)
  , (0, escE, eComBarril)
  ]
-- ─────────────────────────────
-- Runner de feedback
-- ─────────────────────────────
dataTarefa2 :: IO TaskData
dataTarefa2 = do
  let ins = testesT2
  outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
  pure $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
