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

-- | Tipo que representa a seleção de jogada (ação/arma).
nextSelJogada :: SelJogada -> SelJogada
nextSelJogada j = undefined

-- | Seleciona a jogada anterior na lista de jogadas possíveis.
prevSelJogada :: SelJogada -> SelJogada
prevSelJogada j = undefined

-- | Cria o estado inicial do jogo Worms a partir de um Estado.
defaultWorms :: Estado -> Worms
defaultWorms e = undefined

-- | Retorna a lista de índices de worms válidas (não mortos).
wormsValida :: Estado -> [Int]
wormsValida e = undefined

-- Cicla a seleção de worm para a próxima worm válida.
cycleSel :: Estado -> Int -> Int
cycleSel e = undefined
