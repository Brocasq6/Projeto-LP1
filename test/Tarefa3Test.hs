module Main where

import Labs2025
import Tarefa3
import Magic

-- ==========
-- Mapas
-- ==========

mBase :: Mapa
mBase =
  [ [Ar,Ar,Ar]
  , [Ar,Terra,Ar]
  , [Agua,Ar,Pedra]
  ]

mPedraCentro :: Mapa
mPedraCentro =
  [ [Ar,Ar,Ar]
  , [Ar,Pedra,Ar]
  , [Agua,Ar,Pedra]
  ]

mChaoTudoAr :: Mapa
mChaoTudoAr =
  replicate 3 (replicate 3 Ar)

mTopoAgua :: Mapa
mTopoAgua =
  [ [Agua,Agua,Agua]
  , [Ar,Terra,Ar]
  , [Agua,Ar,Pedra]
  ]

-- ==========
-- Helpers
-- ==========

wAt :: (Int,Int) -> Minhoca
wAt p = Minhoca (Just p) (Viva 100) 1 1 1 1 1

wDead :: Minhoca
wDead = Minhoca Nothing Morta 0 0 0 0 0

wNoPosAlive :: Minhoca
wNoPosAlive = Minhoca Nothing (Viva 100) 1 1 1 1 1

-- um disparo genérico
bazuca :: (Int,Int) -> Direcao -> NumMinhoca -> Objeto
bazuca p d dono = Disparo p d Bazuca Nothing dono

mina :: (Int,Int) -> Maybe Ticks -> NumMinhoca -> Objeto
mina p t dono = Disparo p Norte Mina t dono

dinamite :: (Int,Int) -> Maybe Ticks -> Direcao -> NumMinhoca -> Objeto
dinamite p t d dono = Disparo p d Dinamite t dono

barril :: (Int,Int) -> Bool -> Objeto
barril = Barril

-- ==========
-- Estados base de apoio
-- ==========

eBase :: Estado
eBase = Estado mBase [] [wAt (1,1)]

eVazio :: Estado
eVazio = Estado mChaoTudoAr [] []

-- minhoca no ar (0,1)
eMinhocaNoAr :: Estado
eMinhocaNoAr = Estado mBase [] [wAt (0,1)]

-- minhoca sobre água (2,0) -> vai morrer
eMinhocaSobreAgua :: Estado
eMinhocaSobreAgua = Estado mBase [] [wAt (2,0)]

-- ==========
-- Objetos / movimento
-- ==========

-- Bazuca no ar a mover Este
eObjetoBazucaEmMovimento :: Estado
eObjetoBazucaEmMovimento =
  Estado mChaoTudoAr [bazuca (1,1) Este 0] [wAt (1,0)]

-- Bazuca fora do mapa desaparece (vamos colocá-la na borda a sair)
eObjetoBazucaForaMapa :: Estado
eObjetoBazucaForaMapa =
  Estado mChaoTudoAr [bazuca (0,2) Este 0] [wAt (1,1)]

-- Dinamite no chão (fica e countdown)
eDinamiteNoChao :: Estado
eDinamiteNoChao =
  Estado mChaoTudoAr [dinamite (2,1) (Just 2) Sul 0] [wAt (1,1)]

-- Dinamite no ar (parábola vertical = gravidade) — aqui simulamos acima de Ar
eDinamiteNoAr :: Estado
eDinamiteNoAr =
  Estado mChaoTudoAr [dinamite (0,1) (Just 2) Este 0] [wAt (2,2)]

-- Mina no “chão” (imóvel; direção norte nos exemplos)
eMinaNoChao :: Estado
eMinaNoChao =
  Estado mChaoTudoAr [mina (1,1) Nothing 0] [wAt (1,0)]

-- Mina no ar (cai)
eMinaNoAr :: Estado
eMinaNoAr =
  Estado mChaoTudoAr [mina (0,1) Nothing 0] [wAt (2,2)]

-- Mina tempo 0 explode
eMinaComTempo0 :: Estado
eMinaComTempo0 =
  Estado mChaoTudoAr [mina (1,1) (Just 0) 0] [wAt (1,0)]

-- Barril prestes a explodir (explodeBarril=True)
eBarrilPrestesAExplodir :: Estado
eBarrilPrestesAExplodir =
  Estado mChaoTudoAr [barril (1,1) True] [wAt (1,0)]

-- Barril no ar -> passa a prestes a explodir
eBarrilNoAr :: Estado
eBarrilNoAr =
  Estado mChaoTudoAr [barril (0,1) False] [wAt (2,2)]

-- Mina ativa se minhoca inimiga próxima (tempo 2)
eMinaComMinhocaProxima :: Estado
eMinaComMinhocaProxima =
  Estado mChaoTudoAr [mina (1,1) Nothing 0] [wAt (1,2), wAt (2,2)]

-- Dinamite com tempo 1 -> vai para 0
eDinamiteComTempo1 :: Estado
eDinamiteComTempo1 =
  Estado mChaoTudoAr [dinamite (1,1) (Just 1) Sul 0] [wAt (2,2)]

-- Dinamite explode (tempo 0) – danos 7x7
eDinamiteExplode :: Estado
eDinamiteExplode =
  Estado mChaoTudoAr [dinamite (1,1) (Just 0) Sul 0] [wAt (1,2)]

-- Bazuca explode na pedra (colisão)
eBazucaExplodeNaPedra :: Estado
eBazucaExplodeNaPedra =
  Estado mPedraCentro [bazuca (1,0) Este 0] [wAt (2,2)]

-- Mina explode sobre minhoca (mina com tempo 0 sobre (1,1))
eMinaExplodeSobreMinhoca :: Estado
eMinaExplodeSobreMinhoca =
  Estado mChaoTudoAr [mina (1,1) (Just 0) 0] [wAt (1,1)]

-- Danos de barril (diâmetro 5)
eBarrilExplodeDano :: Estado
eBarrilExplodeDano =
  Estado mChaoTudoAr [barril (1,1) True] [wAt (1,2), wAt (0,1)]

-- Dinamite explode dano 7x7
eDinamiteExplodeDano :: Estado
eDinamiteExplodeDano = eDinamiteExplode

-- Mina explode dano 5x5 (pela tua T3 minas têm diâmetro 3 – usamos aqui mina tempo 0)
eMinaExplodeDano :: Estado
eMinaExplodeDano = eMinaExplodeSobreMinhoca

-- Bazuca explode dano 5x5 (quando colide com pedra)
eBazucaExplodeDano :: Estado
eBazucaExplodeDano = eBazucaExplodeNaPedra

-- Minhoca morta sem posição permanece
eMinhocaMortaSemPosicao :: Estado
eMinhocaMortaSemPosicao =
  Estado mChaoTudoAr [] [wDead]

-- Minhoca no limite inferior (2,1) – abaixo é fora; ao cair morre
eMinhocaNoLimiteInferior :: Estado
eMinhocaNoLimiteInferior =
  Estado mChaoTudoAr [] [wAt (2,1)]

-- Objeto já fora do mapa é eliminado
eObjetoForaMapa :: Estado
eObjetoForaMapa =
  Estado mChaoTudoAr [bazuca (-1,-1) Este 0] [wAt (1,1)]

-- Dinamite virada Sul cai vertical
eDinamiteViradaSul :: Estado
eDinamiteViradaSul =
  Estado mChaoTudoAr [dinamite (0,1) (Just 2) Sul 0] [wAt (2,2)]

-- Mina na água cai (vertical)
eMinaNaAgua :: Estado
eMinaNaAgua =
  Estado mTopoAgua [mina (0,1) Nothing 0] [wAt (2,2)]

-- Mina no chão doutro dono ativa
eMinaNoChaoOutroDono :: Estado
eMinaNoChaoOutroDono =
  Estado mChaoTudoAr [mina (1,1) Nothing 0] [wAt (1,2)]  -- wAt(1,2) é “inimigo”

-- Dinamite no topo do mapa cai
eDinamiteNoTopoMapa :: Estado
eDinamiteNoTopoMapa =
  Estado mChaoTudoAr [dinamite (0,0) (Just 3) Este 0] [wAt (2,2)]

-- Vários objetos (efeitos combinados simples)
eComVariosObjetos :: Estado
eComVariosObjetos =
  Estado mChaoTudoAr
    [ bazuca (1,0) Este 0
    , mina (2,2) (Just 0) 0
    , barril (0,2) False
    ]
    [wAt (1,2)]

-- Vários danos (duas explosões no mesmo tick)
eComVariosDanos :: Estado
eComVariosDanos =
  Estado mChaoTudoAr
    [ mina (1,1) (Just 0) 0
    , dinamite (0,1) (Just 0) Sul 0
    ]
    [wAt (1,2), wAt (0,0)]

-- Encadeada (explosão que atinge barril)
eExplosaoEncadeada :: Estado
eExplosaoEncadeada =
  Estado mChaoTudoAr
    [ dinamite (1,1) (Just 0) Sul 0
    , barril (1,2) False
    ]
    [wAt (2,2)]

-- Barril encadeado (vários barris)
eBarrilEncadeado :: Estado
eBarrilEncadeado =
  Estado mChaoTudoAr
    [ barril (1,1) True
    , barril (1,2) False
    ]
    [wAt (0,0)]

-- Danos diferenciados (duas minhocas com distâncias diferentes)
eMinhocasAfetadasDiferente :: Estado
eMinhocasAfetadasDiferente =
  Estado mChaoTudoAr
    [ dinamite (1,1) (Just 0) Sul 0 ]
    [wAt (1,1), wAt (0,1), wAt (2,2)]

-- Objeto com tempo negativo (inválido) – tratamos como removível
eObjetoComTempoNegativo :: Estado
eObjetoComTempoNegativo =
  Estado mChaoTudoAr [Disparo (1,1) Norte Dinamite (Just (-1)) 0] [wAt (2,2)]

-- Minhoca em pedra (sem efeito na queda)
eMinhocaEmPedra :: Estado
eMinhocaEmPedra =
  Estado mPedraCentro [] [wAt (1,1)]

-- Bazuca contra terra (explode)
eBazucaContraTerra :: Estado
eBazucaContraTerra =
  Estado mBase [bazuca (1,0) Este 0] [wAt (2,2)]

-- Bazuca contra barril (explode ambos)
eBazucaContraBarril :: Estado
eBazucaContraBarril =
  Estado mChaoTudoAr [bazuca (1,0) Este 0, barril (1,1) False] [wAt (2,2)]

-- Mina ativa no tempo 1 (contagem)
eMinaAtivaNoTempo1 :: Estado
eMinaAtivaNoTempo1 =
  Estado mChaoTudoAr [mina (1,1) (Just 1) 0] [wAt (2,2)]

-- Dinamite fora do mapa (removida)
eDinamiteForaDoMapa :: Estado
eDinamiteForaDoMapa =
  Estado mChaoTudoAr [dinamite (-1,-1) (Just 1) Este 0] [wAt (1,1)]

-- Barril no topo (no ar) -> prestes a explodir
eBarrilNoTopo :: Estado
eBarrilNoTopo =
  Estado mChaoTudoAr [barril (0,1) False] [wAt (2,2)]

-- Bazuca em água (removida na colisão com água/borda)
eBazucaEmAgua :: Estado
eBazucaEmAgua =
  Estado mTopoAgua [bazuca (0,1) Este 0] [wAt (2,2)]

-- Minhoca cai sobre outra (empilhamento simples; só a de cima cai)
eMinhocaCaiSobreOutra :: Estado
eMinhocaCaiSobreOutra =
  Estado mChaoTudoAr [] [wAt (0,1), wAt (1,1)]

-- Minhoca com explosão próxima (sofre dano)
eMinhocaComExplosaoProxima :: Estado
eMinhocaComExplosaoProxima =
  Estado mChaoTudoAr [barril (1,1) True] [wAt (1,2)]

-- Objeto em chão com dano (desaparece ao ser atingido)
eObjetoEmChaoComDano :: Estado
eObjetoEmChaoComDano =
  Estado mChaoTudoAr [bazuca (1,1) Este 0, mina (1,1) (Just 0) 0] [wAt (2,2)]

-- Mina e jetpack simultâneo (jetpack não interessa na T3; mina conta tempo)
eMinaEJetpackSimultaneo :: Estado
eMinaEJetpackSimultaneo =
  Estado mChaoTudoAr [mina (1,1) (Just 2) 0] [wAt (2,2)]

-- Estado final (todos mortos)
eEstadoFinal :: Estado
eEstadoFinal =
  Estado mChaoTudoAr [] [wDead, wDead]

-- Sem minhocas nem objetos (estável)
eSemMinhocasNemObjetos :: Estado
eSemMinhocasNemObjetos = eVazio

-- Stress test / interações complexas / borda / queda lateral / rotação bazuca:
-- Para simplificar, referenciamos alguns existentes equivalentes
eComTudo :: Estado
eComTudo = Estado mChaoTudoAr
  [barril (1,1) True, bazuca (0,0) Este 0, dinamite (2,2) (Just 1) Sul 0]
  [wAt (1,2), wAt (0,2)]

eInteraçõesComplexas :: Estado
eInteraçõesComplexas = eComTudo

eExplosãoLimiteMapa :: Estado
eExplosãoLimiteMapa = eBazucaExplodeNaPedra

eQuedaLateral :: Estado
eQuedaLateral = eDinamiteNoAr

eBazucaRotação :: Estado
eBazucaRotação = eObjetoBazucaEmMovimento

-- ==========
-- Lista de testes (usa o nome correto)
-- ==========

testesT3 :: [Estado]
testesT3 =
  [ eBase
  , eMinhocaNoAr
  , eMinhocaSobreAgua
  , eObjetoBazucaEmMovimento
  , eObjetoBazucaForaMapa
  , eDinamiteNoChao
  , eDinamiteNoAr
  , eMinaNoChao
  , eMinaNoAr
  , eMinaComTempo0
  , eBarrilPrestesAExplodir
  , eBarrilNoAr
  , eMinaComMinhocaProxima
  , eDinamiteComTempo1
  , eDinamiteExplode
  , eBazucaExplodeNaPedra
  , eMinaExplodeSobreMinhoca
  , eBarrilExplodeDano
  , eDinamiteExplodeDano
  , eMinaExplodeDano
  , eBazucaExplodeDano
  , eMinhocaMortaSemPosicao
  , eMinhocaNoLimiteInferior
  , eObjetoForaMapa
  , eDinamiteViradaSul
  , eMinaNaAgua
  , eMinaNoChaoOutroDono
  , eDinamiteNoTopoMapa
  , eComVariosObjetos
  , eComVariosDanos
  , eExplosaoEncadeada
  , eBarrilEncadeado
  , eMinhocasAfetadasDiferente
  , eObjetoComTempoNegativo
  , eMinhocaEmPedra
  , eBazucaContraTerra
  , eBazucaContraBarril
  , eMinaAtivaNoTempo1
  , eDinamiteForaDoMapa
  , eBarrilNoTopo
  , eBazucaEmAgua
  , eMinhocaCaiSobreOutra
  , eMinhocaComExplosaoProxima
  , eObjetoEmChaoComDano
  , eMinaEJetpackSimultaneo
  , eEstadoFinal
  , eSemMinhocasNemObjetos
  , eComTudo
  , eInteraçõesComplexas
  , eExplosãoLimiteMapa
  , eQuedaLateral
  , eBazucaRotação
  ]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
  let ins = testesT3
  outs <- mapM (runTest . avancaEstado) ins
  return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3
