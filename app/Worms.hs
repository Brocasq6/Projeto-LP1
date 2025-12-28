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
    { jogo
    , selW 
    , selJ 
    }

nextSelJogada :: SelJogada -> SelJogada
nextSelJogada j = undefined

prevSelJogada :: SelJogada -> SelJogada
prevSelJogada j = undefined

defaultWorms :: Estado -> Worms
defaultWorms e = undefined

wormsValida :: Estado -> [Int]
wormsValida e = undefined

cycleSel :: Estado -> Int -> Int
cycleSel e = undefined
