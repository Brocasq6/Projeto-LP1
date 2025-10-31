module Main where

import Labs2025
import Tarefa3
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 3
testesT3 :: [Estado]
testesT3 =
  [ eBase                         -- nada muda
  , eMinhocaNoAr                  -- minhoca cai
  , eMinhocaSobreAgua             -- minhoca morre
  , eObjetoBazucaEmMovimento      -- bazuca avança
  , eObjetoBazucaForaMapa         -- bazuca some
  , eDinamiteNoChao               -- fica e countdown
  , eDinamiteNoAr                 -- faz parábola
  , eMinaNoChao                   -- imobiliza e vira norte
  , eMinaNoAr                     -- cai
  , eMinaComTempo0                -- explode
  , eBarrilPrestesAExplodir       -- explode
  , eBarrilNoAr                   -- passa a prestes a explodir
  , eMinaComMinhocaProxima        -- ativa tempo 2
  , eDinamiteComTempo1            -- decresce para 0
  , eDinamiteExplode              -- gera danos
  , eBazucaExplodeNaPedra         -- desaparece
  , eMinaExplodeSobreMinhoca      -- dano na minhoca
  , eBarrilExplodeDano            -- dano circular
  , eDinamiteExplodeDano          -- dano 7x7
  , eMinaExplodeDano              -- dano 5x5
  , eBazucaExplodeDano            -- dano 5x5
  , eMinhocaMortaSemPosicao       -- permanece igual
  , eMinhocaNoLimiteInferior      -- morre
  , eObjetoForaMapa               -- eliminado
  , eDinamiteViradaSul            -- cai vertical
  , eMinaNaAgua                   -- cai vertical
  , eMinaNoChaoOutroDono          -- ativa
  , eDinamiteNoTopoMapa           -- cai
  , eComVariosObjetos             -- múltiplos efeitos
  , eComVariosDanos               -- múltiplos danos
  , eExplosaoEncadeada            -- dano em série
  , eBarrilEncadeado              -- múltiplas explosões
  , eMinhocasAfetadasDiferente    -- danos diferenciados
  , eObjetoComTempoNegativo       -- inválido, removido
  , eMinhocaEmPedra               -- sem efeito
  , eBazucaContraTerra            -- explode
  , eBazucaContraBarril           -- explode ambos
  , eMinaAtivaNoTempo1            -- countdown
  , eDinamiteForaDoMapa           -- removida
  , eBarrilNoTopo                 -- prestes a explodir
  , eBazucaEmAgua                 -- removida
  , eMinhocaCaiSobreOutra         -- empilhamento
  , eMinhocaComExplosaoProxima    -- sofre dano
  , eObjetoEmChaoComDano          -- desaparece
  , eMinaEJetpackSimultaneo       -- jetpack ignora tempo
  , eEstadoFinal                  -- todos mortos
  , eSemMinhocasNemObjetos        -- estável
  , eComTudo                      -- stress test
  , eInteraçõesComplexas          -- múltiplas leis
  , eExplosãoLimiteMapa           -- bordas
  , eQuedaLateral                 -- física diagonal
  , eBazucaRotação                -- direção muda
  ]


dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3