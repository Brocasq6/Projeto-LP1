module Tempo where

import Worms
import Tarefa3


-- | Tempo em segundos.
type Segundos = Float

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> Worms
reageTempo _ w =
  case screen w of
    EmJogo ->
      let e'  = avancaEstado (jogo w)
          w'  = w { jogo = e' }
      in w' { selW = cycleSelW e' (selW w') }
    _ -> w
