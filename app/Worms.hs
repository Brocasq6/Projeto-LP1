module Worms where

{-
Worms.hs
 |- data Worms = Worms
 |    { jogo
 |    , selW 
 |    , selJ 
 |    , ... (opcional: dtAcc, camera, mensagem)
 |    }
 |
 | data SelJogada
 |    = ... (lista de ações/armas)
 |
 |- nextSelJogada 
 |- prevSelJogada 
 |
 |- defaultWorms 
 |    └─ cria World a partir de um Estado inicial
 |
 |- wormsValidas                     
 |- cycleSelW                   
-}

-- | Estado do jogo no Gloss.
data Worms = Worms 
    { jogo :: Estado
    , selW :: Int
    , selJ :: SelJogada
    }

-- | Função que avança para a próxima jogada selecionada.
nextSelJogada :: SelJogada -> SelJogada
nextSelJogada j
  | j == maxBound = minBound
  | otherwise     = succ j

-- | Seleciona a jogada anterior na lista de jogadas possíveis.
prevSelJogada :: SelJogada -> SelJogada
prevSelJogada j = 
  | j == minbound = maxBound
  | otherwise     = pred j

-- | Cria o estado inicial do jogo Worms a partir de um Estado.
defaultWorms :: Estado -> Worms
defaultWorms e = 
    Worms 
      { jogo = e
      , selW = primeiraMinhocaValida e
      , selJ = minBound
      }

-- | Retorna o índice da primeira minhoca válida no estado do jogo.
primeiraMinhocaValida :: Estado -> Int
primeiraMinhocaValida e =
  case wormsValidas e of
    []    -> 0
    (x:_) -> x

-- | Função que retorna as minhocas válidas no estado do jogo.
wormsValidas :: Estado -> [Int]
wormsValidas e =
  [ i | (i,m) <- zip [0..] (minhocasEstado e)
  , posicaoMinhoca m /= Nothing
  , vidaMinhoca m /= Morta
  ]

-- Cicla a seleção de worm para a próxima worm válida.
cycleSel :: Estado -> Int -> Int
cycleSel e atual = 
  case wormsValidas e of 
    [] -> atual
    ws -> 
      let k = case elemIndex atual ws of
                Just i  -> (i + 1) `mod` length ws
                Nothing -> 0
      in ws !! k

