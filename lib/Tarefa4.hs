{-|
Module      : Tarefa4
Description : Implementar uma tática de jogo.

Módulo para a realização da Tarefa 4 de LI1\/LP1 em 2025\/26.
-}
module Tarefa4 where

import Data.Either

import Labs2025
import Tarefa2
import Tarefa3

-- | Função principal da Tarefa 4. Dado um estado retorna uma lista de jogadas, com exatamente 100 jogadas.
tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e,[]) [0..99]

-- | Aplica uma sequência de jogadas a um estado, avançando o tempo entre jogadas.
avancaTatica :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
avancaTatica (e,js) tick = (avancaJogada j e,j:js)
    where j = jogadaTatica tick e

-- | Aplica uma jogada de uma minhoca a um estado, e avança o tempo.
avancaJogada :: (NumMinhoca,Jogada) -> Estado -> Estado
avancaJogada (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

-- | Avança o tempo para o estado de uma minhoca, se não efetuou a última jogada.
avancaMinhocaJogada :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
avancaMinhocaJogada e (i,minhoca,minhoca')
    | posicaoMinhoca minhoca == posicaoMinhoca minhoca' = avancaMinhoca e i minhoca'
    | otherwise = minhoca'

-- | Avança o tempo para o estado de um objeto, se não foi criado pela última jogada.
avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i,objeto')
  | elem objeto' (objetosEstado e) = avancaObjeto e i objeto'
  | otherwise = Left objeto'











-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca,Jogada)
jogadaTatica t e =
  let i = escolheMinhoca t e
      j = decideJogada t i e
  in (i,j)

-- | Se houver minhocas vivas, alterna entre elas; senão devolve 0
escolheMinhoca :: Ticks -> Estado -> NumMinhoca
escolheMinhoca t e =
  case [ i | (i,_,_) <- minhocasVivasComPos e ] of
    [] -> 0
    is -> is !! (t `mod` length is)

-- | Minhocas "usáveis": vivas e com posição
minhocasVivasComPos :: Estado -> [(NumMinhoca, Minhoca, Posicao)]
minhocasVivasComPos (Estado _ _ ms) =
  [ (i,m,p)
  | (i,m) <- zip [0..] ms
  , estaViva m
  , Just p <- [posicaoMinhoca m]
  ]

-- | Verifica se a minhoca está viva.
estaViva :: Minhoca -> Bool
estaViva m =
  case vidaMinhoca m of
    Viva _ -> True
    Morta  -> False

-- | Decide a jogada para a minhoca escolhida
decideJogada :: Ticks -> NumMinhoca -> Estado -> Jogada
decideJogada t i e@(Estado _ objs ms)
  | i < 0 || i >= length ms = Move (dirCiclo t)
  | otherwise =
      case posicaoMinhoca (ms !! i) of
        Nothing -> Move (dirCiclo t)
        Just p  ->
          case alvoMaisProximo i p e of
            Nothing ->
              -- Sem alvo: tenta mexer um bocado para não ficar sempre igual
              Move (dirCiclo t)
            Just (pAlvo, dir) ->
              -- Prioridade: disparar se "puder agir" e tiver munição e não houver disparo igual ativo
              if podeDisparar i e Bazuca
                 then Dispara Bazuca dir
              else if podeDisparar i e Dinamite
                 then Dispara Dinamite dir
              else if podeDisparar i e Mina
                 then Dispara Mina dir
              else
                -- Caso não dispare, tenta mover-se na direção do alvo
                Move dir

-- | Alvo: minhoca viva mais próxima (diferente da atual)
alvoMaisProximo :: NumMinhoca -> Posicao -> Estado -> Maybe (Posicao, Direcao)
alvoMaisProximo i p e =
  let candidatos =
        [ p2
        | (j,_,p2) <- minhocasVivasComPos e
        , j /= i
        ]
  in case candidatos of
       [] -> Nothing
       ps ->
         let pAlvo = minimoPor (distManhattan p) ps
             dir   = direcaoPara p pAlvo
         in Just (pAlvo, dir)

-- | Distância Manhattan entre duas posições.
distManhattan :: Posicao -> Posicao -> Int
distManhattan (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

-- | Retorna o mínimo de uma lista segundo uma função de ordenação.
minimoPor :: Ord b => (a -> b) -> [a] -> a
minimoPor f (x:xs) = foldl (\best a -> if f a < f best then a else best) x xs
minimoPor _ []     = error "minimoPor: lista vazia"

-- | Verifica se a minhoca i pode disparar a arma (segundo as regras da vossa T2)
podeDisparar :: NumMinhoca -> Estado -> TipoArma -> Bool
podeDisparar i e@(Estado _ objs ms) arma
  | i < 0 || i >= length ms = False
  | not (estaViva (ms !! i)) = False
  | not (podeAgir e (ms !! i)) = False
  | not (temMunicao arma (ms !! i)) = False
  | existeMesmoDisparo arma i objs = False
  | otherwise = True

-- | Direção "boa o suficiente" para ir de p -> q (8 direções)
direcaoPara :: Posicao -> Posicao -> Direcao
direcaoPara (x1,y1) (x2,y2) =
  let dx = sinal (x2 - x1)
      dy = sinal (y2 - y1)
  in case (dx,dy) of
       (-1, 0) -> Norte
       ( 1, 0) -> Sul
       ( 0, 1) -> Este
       ( 0,-1) -> Oeste
       (-1, 1) -> Nordeste
       (-1,-1) -> Noroeste
       ( 1, 1) -> Sudeste
       ( 1,-1) -> Sudoeste
       ( 0, 0) -> Este

-- | Sinal de um número inteiro
sinal :: Int -> Int
sinal n | n < 0     = -1
        | n > 0     =  1
        | otherwise =  0

-- | Direção cíclica para quando não há alvos / fallback
dirCiclo :: Ticks -> Direcao
dirCiclo t =
  case t `mod` 8 of
    0 -> Este
    1 -> Oeste
    2 -> Norte
    3 -> Sul
    4 -> Nordeste
    5 -> Noroeste
    6 -> Sudeste
    _ -> Sudoeste

