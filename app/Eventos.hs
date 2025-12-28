module Eventos where

import Graphics.Gloss.Interface.Pure.Game

import Worms
{-
Eventos.hs
 |- reageEventos :: Event -> Worms -> Worms
 │    |- (tecla '1')  -> selecionaMinhocaSeguinte
 │    |- (tecla '2')  -> selecionaJogadaSeguinte
 │    |- (W/A/S/D)    -> aplicaJogadaDirecional
 │    |- (outras)     -> mantém estado
 │
 |- selecionaMinhocaSeguinte :: Worms -> Worms
 |    |- wormsValidas :: Estado -> [Int]
 |    |- cycleSelW    :: Estado -> Int -> Int
 |
 |- selecionaJogadaSeguinte :: Worms -> Worms
 |    |- nextSelJogada :: SelJogada -> SelJogada
 |
 |- aplicaJogadaDirecional :: Char -> Worms -> Worms
 |    |- dirFromKey     :: Char -> Maybe Direcao
 |    |- jogadaFromSel  :: SelJogada -> Direcao -> Jogada
 |    |- aplicaEfetua   :: Int -> Jogada -> Estado -> Estado
 |         |- efetuaJogada (T2)
 |
 |- dirFromKey :: Char -> Maybe Direcao
 |- jogadaFromSel :: SelJogada -> Direcao -> Jogada
 |- wormsValidas :: Estado -> [Int]

-}


-- | Função que altera o estado do jogo no Gloss.
reageEventos :: Event -> Worms -> Worms
reageEventos evento w =
  case evento of
    EventKey (Char '1') Down modifiers mousePos -> selecionaMinhocaSeguinte w
    EventKey (Char '2') Down modifiers mousePos -> selecionaJogadaSeguinte w
    EventKey (Char c)   Down modifiers mousePos | c `elem` "wasdWASD" -> aplicaJogadaDirecional (toLowerASCIISafe c) w
    _ -> w
  where
    toLowerASCIISafe ch = case ch of
        'W' -> 'w';
        'A' -> 'a';
        'S' -> 's';
        'D' -> 'd'
        _   -> ch


-- | Função que avança para a próxima minhoca selecionada.
selecionaMinhocaSeguinte :: Worms -> Worms
selecionaMinhocaSeguinte w = 
    let e = jogo w
        x = selW w
        novaSel = cycleSelW e x
    in w { selW = novaSel }

-- | Função que retorna as minhocas válidas no estado do jogo.
wormsValidas :: Estado -> [Int]
wormsValidas e =
  [ i
  | (i,m) <- zip [0..] (minhocasEstado e)
  , posicaoMinhoca m /= Nothing
  , vidaMinhoca m /= Morta
  ]

-- | Função que avança para a próxima minhoca selecionada.
cycleSelW :: Estado -> Int -> Int
cycleSelW e atual =
  case wormsValidas e of
    [] -> 0
    ws ->
      let k = case elemIndex atual ws of
                Nothing -> 0
                Just j  -> (j + 1) `mod` length ws
      in ws !! k

-- | Função que avança para a próxima minhoca selecionada.
selecionaJogadaSeguinte :: Worms -> Worms
selecionaJogadaSeguinte w = w { selJ = nextSelJogada (selJ w) }

-- | Função que avança para a próxima jogada selecionada.
nextSelJogada :: SelJogada -> SelJogada
nextSelJogada j
  | j == maxBound = minBound
  | otherwise     = succ j

-- | Função que converte uma tecla em direção.
dirFromKey :: Char -> Maybe Direcao
dirFromKey c =
    case c of
        'w' -> Just Cima
        'a' -> Just Esquerda
        's' -> Just Baixo
        'd' -> Just Direita
        _   -> Nothing

-- | Função que aplica uma jogada direcional ao estado do jogo.
aplicaJogadaDirecional :: Char -> Worms -> Worms
aplicaJogadaDirecional c w = 
    case dirFromKey c of
        Nothing -> w
        Just d  ->
            let n = selW w
                j = selJ w 
                e = jogo w
                e' = aplicaEfetua n j e
            in w { jogo = e' }



-- | Função que converte uma seleção de jogada e direção em jogada.
jogadaFromSel :: SelJogada -> Direcao -> Jogada
jogadaFromSel j d =
  case j of
    -- movimento (ignora o d recebido e usa a direção “fixa” do seletor)
    MoveUp    -> Movimenta Norte
    MoveDown  -> Movimenta Sul
    MoveRight -> Movimenta Este
    MoveLeft  -> Movimenta Oeste

    -- armas existentes
    UsaJetpack    -> UsaJetpack d
    UsaEscavadora -> UsaEscavadora d
    DisparaBazuca -> Dispara Bazuca d
    LargaMina     -> Dispara Mina d
    LargaDinamite -> Dispara Dinamite d

         


-- | Função que aplica uma jogada ao estado do jogo.
aplicaEfetua :: Int -> Jogada -> Estado -> Estado
aplicaEfetua x j e = efetuaJogada x j e


