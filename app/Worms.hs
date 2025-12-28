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
data Worms = Worms {}
