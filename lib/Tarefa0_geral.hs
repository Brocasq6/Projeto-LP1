{-|
Module      : Tarefa0_geral
Description : Funções auxiliares gerais.

Módulo que define funções genéricas sobre listas e matrizes.
-}

module Tarefa0_geral where

-- * Tipos de dados

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/matriz.png>>
type Matriz a = [[a]]

-- | Uma posição numa matriz é dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/posicaomatriz.png>>
type Posicao = (Int,Int)

-- | A dimensão de uma matrix dada como um par (/número de linhas/,/número de colunhas/).
type Dimensao = (Int,Int)

-- | Uma direção é dada pela rosa dos ventos. Ou seja, os 4 pontos cardeais e os 4 pontos colaterais.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rosadosventos.jpg>>
data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)

-- * Funções não-recursivas.

-- | Verifica se o indice pertence à lista.

eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x l
    | x >= 0 && x < length l = True
    | otherwise = False

-- | Calcula a dimensão de uma matriz.

dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz m = (length m ,length (head m))

-- | Verifica se a posição pertence à matriz.

ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (a,b) l = a <= length l && b <= length (head l)


-- | Move uma posição uma unidade no sentido de uma direção.

movePosicao :: Direcao -> Posicao -> Posicao
movePosicao direcao (l,c) = 
    case direcao of
        Norte -> (l, c-1)
        Nordeste -> (l-1, c+1)
        Este -> (l, c+1)
        Sudeste -> (l+1, c+1)
        Sul -> (l+1, c)
        Sudoeste -> (l+1, c-1)
        Oeste -> (l, c-1)
        Noroeste -> (l-1, c-1)


-- | Versão da função 'movePosicao' que garante que o movimento não se desloca para fora de uma janela.

movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (d1,d2) d (l,c)
    | movePosicao d (l,c) > (d1,d2) = (l,c)
    | movePosicao d (l,c) < (d1,d2) = (l,c)
    | otherwise = movePosicao d (l,c)

-- | Converte uma posição no referencial em que a origem é no canto superior esquerdo da janela numa posição em que a origem passa a estar no centro da janela.

origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (d1,d2) (l,c)  = (l - ((d1 - 1) `div` 2), c - ((d2 - 1) `div` 2))

-- | Roda um par (posição,direção) 45% para a direita.

rodaPosicaoDirecao :: (Posicao, Direcao) -> (Posicao, Direcao)
rodaPosicaoDirecao (pos, direcao) = 
    case direcao of
        Norte -> (movePosicao Norte pos, Nordeste)
        Nordeste -> (movePosicao Nordeste pos, Este)
        Este -> (movePosicao Este pos, Sudeste)
        Sudeste -> (movePosicao Sudeste pos, Sul)
        Sul -> (movePosicao Sul pos, Sudoeste)
        Sudoeste -> (movePosicao Sudoeste pos, Oeste)
        Oeste -> (movePosicao Oeste pos, Noroeste)
        Noroeste -> (movePosicao Noroeste pos, Norte)
        
-------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | Devolve o elemento num dado índice de uma lista.

encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista _ [] = Nothing
encontraIndiceLista 0 (h:t) = Just h
encontraIndiceLista x (h:t)
    | x < 0 = Nothing
    | otherwise = encontraIndiceLista (x-1) t  -- (x-1) faz com que passemos para o proximo indice


-- | Modifica um elemento num dado índice.

atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista n x (h:t)
    | n < 0 = h:t
    | n == 0 = x:t
    | otherwise = h : atualizaIndiceLista (n-1) x t  -- atualizaIndice 3, para o valor 4, na lista [1,2,2,2]  = [1,2,2,4]

-- | Devolve o elemento numa dada posição de uma matriz.

encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz (_,_) [] = Nothing
encontraPosicaoMatriz (l,c) m
    | l >= length m = Nothing
    | c >= length (head m) = Nothing
    | otherwise = Just ((m !! l) !! c) -- funcao que acede á primeira sublista de uma matriz e dps á coluna


-- | Modifica um elemento numa dada posição de uma matriz.
atualizaPosicaoMatriz :: (Eq a) => Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) a m
    | ((m !! l) !! c) == a = m -- se o elemento que queremos atualizar for igual ao que ja existe na matriz, devolve a propria matriz
    | otherwise = atualizaIndiceLista l novaLinha m
        where
            linha = m !! l
            novaLinha = atualizaIndiceLista c a linha



-- | Aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []
moveDirecaoPosicoes d (h:t) = movePosicao d h : moveDirecaoPosicoes d t


-- | Verifica se uma matriz é válida, no sentido em que modela um rectângulo.
eMatrizValida :: Matriz a -> Bool
eMatrizValida m
    | length (head m) == length m = True
    | otherwise = False
