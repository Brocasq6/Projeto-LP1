module Worms where
import Data.List (elemIndex)
import Labs2025


-- | Estado do jogo no Gloss.
data Worms = Worms
  { jogo      :: Estado
  , selW      :: Int
  , selJ      :: SelJogada
  , screen    :: Screen
  , menuSel   :: MenuItem
  , assets    :: Assets
  }


-- | Jogadas possíveis selecionáveis pelo jogador
data SelJogada
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | UsaJetpack
  | UsaEscavadora
  | DisparaBazuca
  | LargaMina
  | LargaDinamite
  deriving (Eq, Enum, Bounded, Show)


-- | Telas do jogo 
data Screen
  = Menu
  | Tutorial
  | Creditos
  | EmJogo
  deriving (Eq, Show)


-- | Itens do menu principal
data MenuItem
  = MenuJogar
  | MenuTutorial
  | MenuCreditos
  deriving (Eq, Enum, Bounded, Show)

-- | Avança para o próximo item do menu (ciclando).
nextMenuItem :: MenuItem -> MenuItem
nextMenuItem x | x == maxBound = minBound
               | otherwise     = succ x

-- | Retrocede para o item anterior do menu (ciclando).
prevMenuItem :: MenuItem -> MenuItem
prevMenuItem x | x == minBound = maxBound
               | otherwise     = pred x

-- | Estrutura que contém os assets do jogo.
data Assets = Assets
  { menuBg :: Picture
  }

-- | Função que avança para a próxima jogada selecionada.
nextSelJogada :: SelJogada -> SelJogada
nextSelJogada j
  | j == maxBound = minBound
  | otherwise     = succ j

-- | Seleciona a jogada anterior na lista de jogadas possíveis.
prevSelJogada :: SelJogada -> SelJogada
prevSelJogada j 
  | j == minBound = maxBound
  | otherwise     = pred j

-- | Cria o estado inicial do jogo Worms a partir de um Estado.
defaultWorms :: Estado -> Worms
defaultWorms a e =
  Worms
    { jogo    = e
    , selW    = primeiraMinhocaValida e
    , selJ    = MoveUp
    , screen  = EmJogo
    , menuSel = MenuJogar
    , assets  = a
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

-- | Cicla a seleção de worm para a próxima worm válida.
cycleSelW :: Estado -> Int -> Int
cycleSelW e atual = 
  case wormsValidas e of 
    [] -> atual
    ws -> 
      let k = case elemIndex atual ws of
                Just i  -> (i + 1) `mod` length ws
                Nothing -> 0
      in ws !! k
