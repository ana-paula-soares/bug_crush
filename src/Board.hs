module Board where

import Types
import System.Random (randomRIO)

-- 1. Acessores e Modificadores (Getters/Setters)

-- Pega um item (seguro)
get :: Board -> Coord -> Candy
get board (r, c)
    | r < 0 || r >= height || c < 0 || c >= width = Empty
    | otherwise = (board !! r) !! c

-- Altera um item
set :: Board -> Coord -> Candy -> Board
set board (r, c) val = replaceList r newRow board
    where newRow = replaceList c val (board !! r)

-- Função auxiliar genérica para listas
replaceList :: Int -> a -> [a] -> [a]
replaceList _ _ [] = []
replaceList i val (x:xs)
    | i == 0    = val : xs
    | otherwise = x : replaceList (i - 1) val xs

-- 2. Geradores Aleatórios (IO)

-- Cria um doce aleatório
randomCandy :: IO Candy
randomCandy = do
    r <- randomRIO (0, 4) :: IO Int
    return $ case r of 0 -> Red; 1 -> Blue; 2 -> Green; 3 -> Yellow; _ -> Purple

-- Gera tabuleiro inicial BRUTO (pode ter matches, o Main vai corrigir)
generateBoard :: IO Board
generateBoard = do
    mapM (\_ -> mapM (\_ -> randomCandy) [1..width]) [1..height]