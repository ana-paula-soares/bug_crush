module Types where

-- Tipos de Doces
data Candy = Red | Blue | Green | Yellow | Purple | Empty
    deriving (Eq, Show)

-- O Tabuleiro
type Board = [[Candy]]

-- Coordenadas (Linha, Coluna)
type Coord = (Int, Int)

-- Constantes Globais
width, height :: Int
width = 8
height = 8