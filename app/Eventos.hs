module Eventos where

import Graphics.Gloss.Interface.Pure.Game

import Worms
import Labs2025
import Tarefa2

-- | Função que altera o estado do jogo no Gloss.
reageEventos :: Event -> Worms -> Worms
reageEventos ev w =
  case screen w of
    Menu     -> eventosMenu ev w
    Tutorial -> eventosTexto ev w
    Creditos -> eventosTexto ev w
    EmJogo   -> eventosJogo ev w

-- | Função que trata os eventos na tela do menu.
eventosMenu :: Event -> Worms -> Worms
eventosMenu ev w =
  case ev of
    EventKey (SpecialKey KeyUp) Down _ _   -> w { menuSel = prevMenuItem (menuSel w) }
    EventKey (SpecialKey KeyDown) Down _ _ -> w { menuSel = nextMenuItem (menuSel w) }
    EventKey (Char 'w') Down _ _           -> w { menuSel = prevMenuItem (menuSel w) }
    EventKey (Char 's') Down _ _           -> w { menuSel = nextMenuItem (menuSel w) }

    EventKey (SpecialKey KeyEnter) Down _ _ -> confirmaMenu w
    EventKey (SpecialKey KeySpace) Down _ _ -> confirmaMenu w
    _ -> w

-- | Função que confirma a seleção do menu e altera a tela.
confirmaMenu :: Worms -> Worms
confirmaMenu w =
  case menuSel w of
    MenuJogar    -> w { screen = EmJogo }
    MenuTutorial -> w { screen = Tutorial }
    MenuCreditos -> w { screen = Creditos }

-- | Função que trata os eventos na tela de texto (tutorial/créditos).
eventosTexto :: Event -> Worms -> Worms
eventosTexto ev w =
  case ev of
    EventKey (SpecialKey KeyEsc) Down _ _ -> w { screen = Menu }
    _ -> w

-- | Função que trata os eventos durante o jogo.
eventosJogo :: Event -> Worms -> Worms
eventosJogo evento w =
  case evento of
    EventKey (SpecialKey KeyEsc) Down _ _ -> w { screen = Menu }

    EventKey (Char '1') Down _ _ -> selecionaMinhocaSeguinte w
    EventKey (Char '2') Down _ _ -> selecionaJogadaSeguinte w
    EventKey (Char c)   Down _ _ | c `elem` "wasdWASD" -> aplicaJogadaDirecional (toLowerASCIISafe c) w
    _ -> w
  where
    toLowerASCIISafe ch = case ch of
      'W' -> 'w'
      'A' -> 'a'
      'S' -> 's'
      'D' -> 'd'
      _   -> ch

-- | Função que avança para a próxima minhoca selecionada.
selecionaMinhocaSeguinte :: Worms -> Worms
selecionaMinhocaSeguinte w = 
    let e = jogo w
        x = selW w
        novaSel = cycleSelW e x
    in w { selW = novaSel }

-- | Função que avança para a próxima minhoca selecionada.
selecionaJogadaSeguinte :: Worms -> Worms
selecionaJogadaSeguinte w = w { selJ = nextSelJogada (selJ w) }

-- | Função que converte uma tecla em direção.
dirFromKey :: Char -> Maybe Direcao
dirFromKey c =
  case c of
    'w' -> Just Norte
    'a' -> Just Oeste
    's' -> Just Sul
    'd' -> Just Este
    _   -> Nothing


-- | Função que aplica uma jogada direcional ao estado do jogo.
aplicaJogadaDirecional :: Char -> Worms -> Worms
aplicaJogadaDirecional c w =
  case dirFromKey c of
    Nothing -> w
    Just d  ->
      w { jogo = efetuaJogada (selW w) (Move d) (jogo w) }

-- | Função que converte uma seleção de jogada e direção em jogada.
jogadaFromSel :: SelJogada -> Direcao -> Jogada
jogadaFromSel j d =
  case j of
    -- movimento (ignora o d recebido e usa a direção “fixa” do seletor)
    MoveUp    -> Move Norte
    MoveDown  -> Move Sul
    MoveRight -> Move Este
    MoveLeft  -> Move Oeste

    -- armas existentes
    UsaJetpack    -> Dispara Jetpack d
    UsaEscavadora -> Dispara Escavadora d
    DisparaBazuca -> Dispara Bazuca d
    LargaMina     -> Dispara Mina d
    LargaDinamite -> Dispara Dinamite d

-- | Função que aplica uma jogada ao estado do jogo.
aplicaEfetua :: Int -> Jogada -> Estado -> Estado
aplicaEfetua x j e = efetuaJogada x j e


