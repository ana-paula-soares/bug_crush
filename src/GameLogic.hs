module GameLogic where

import Types
import Board (get, set, randomCandy, replaceList)
import Data.List (nub, intersect, partition)

-- 1. Regras Puras

swap :: Board -> Coord -> Coord -> Board
swap board c1 c2 =
    let v1 = get board c1
        v2 = get board c2
    in set (set board c1 v2) c2 v1

isValidMove :: Coord -> Coord -> Bool
isValidMove (r1, c1) (r2, c2) =
    abs (r1 - r2) + abs (c1 - c2) == 1

-- 2. Busca de Matches

-- Retorna todas as coordenadas que fazem parte de algum match (horizontal ou vertical)
findMatches :: Board -> [Coord]
findMatches board = nub (horizontal ++ vertical)
  where
    horizontal =
        [ (r, c+k)
        | r <- [0 .. height-1]
        , c <- [0 .. width-3]
        , let a = get board (r, c)
        , let b = get board (r, c+1)
        , let d = get board (r, c+2)
        , a /= Empty, a == b, b == d
        , k <- [0,1,2]
        ]

    vertical =
        [ (r+k, c)
        | r <- [0 .. height-3]
        , c <- [0 .. width-1]
        , let a = get board (r, c)
        , let b = get board (r+1, c)
        , let d = get board (r+2, c)
        , a /= Empty, a == b, b == d
        , k <- [0,1,2]
        ]

-- Limpa todas as coordenadas que fazem parte de um match
clearMatches :: Board -> [Coord] -> Board
clearMatches board [] = board
clearMatches board (c:cs) =
    clearMatches (set board c Empty) cs

-- 3. Física (queda das peças)

stepGravity :: Board -> IO (Board, Bool)
stepGravity board = do
    let topRow = board !! 0
    newTopRow <- mapM (\c -> if c == Empty then randomCandy else return c) topRow
    let boardWithTop = replaceList 0 newTopRow board

    let (finalBoard, moved) =
            foldl processRow (boardWithTop, False) [height-1, height-2 .. 1]

    return (finalBoard, moved || topRow /= newTopRow)
  where
    processRow (b, changed) r =
        foldl
            (\(cb, ch) c ->
                let me = get cb (r, c)
                    up = get cb (r-1, c)
                in if me == Empty && up /= Empty
                   then (set (set cb (r, c) up) (r-1, c) Empty, True)
                   else (cb, ch)
            )
            (b, changed)
            [0 .. width-1]

-- 4. Correção Inicial

-- Remove matches do tabuleiro inicial para evitar pontos automáticos
fixInitialMatches :: Board -> IO Board
fixInitialMatches board = do
    let ms = findMatches board
    if null ms
        then return board
        else do
            newBoard <- replaceAtCoords board ms
            fixInitialMatches newBoard

replaceAtCoords :: Board -> [Coord] -> IO Board
replaceAtCoords board [] = return board
replaceAtCoords board (c:cs) = do
    newCandy <- randomCandy
    let safeCandy = if newCandy == Empty then Red else newCandy
    replaceAtCoords (set board c safeCandy) cs

-- 5. Agrupamento de Combinações

-- Recebe uma lista de coordenadas e separa em grupos horizontais e verticais
matches :: [Coord] -> [[Coord]]
matches [] = []
matches coords =
    horizontalGroups ++ verticalGroups
  where
    horizontalGroups =
        [ g
        | r <- [0..height-1]
        , g <- findSegments [(r, c) | c <- [0..width-1]] coords
        ]

    verticalGroups =
        [ g
        | c <- [0..width-1]
        , g <- findSegments [(r, c) | r <- [0..height-1]] coords
        ]

-- Extrai segmentos consecutivos de uma linha ou coluna
findSegments :: [Coord] -> [Coord] -> [[Coord]]
findSegments line pool =
    filter (\g -> length g >= 3) (foldr groupConsecutive [] present)
  where
    present = line `intersect` pool

-- Agrupa coordenadas adjacentes
groupConsecutive :: Coord -> [[Coord]] -> [[Coord]]
groupConsecutive c [] = [[c]]
groupConsecutive c (g:gs)
    | isAdjacent c (head g) = (c:g) : gs
    | otherwise            = [c] : (g:gs)

-- Verifica se duas coordenadas são vizinhas
isAdjacent :: Coord -> Coord -> Bool
isAdjacent (r1,c1) (r2,c2) =
    abs (r1-r2) + abs (c1-c2) == 1

-- 6. União de Grupos Cruzados

-- Une grupos que compartilham pelo menos uma coordenada
mergeGroups :: [[Coord]] -> [[Coord]]
mergeGroups [] = []
mergeGroups (g:gs) = 
    let (related, others) = partition (any (`elem` g)) gs
    in if null related
       then g : mergeGroups others
       else mergeGroups (nub (g ++ concat related) : others)

-- Função principal de detecção de grupos válidos no tabuleiro
detectGroups :: Board -> [[Coord]]
detectGroups board =
    mergeGroups (matches (findMatches board))

-- 7. Pontuação

-- Calcula a pontuação de um grupo de acordo com seu tamanho
points :: [Coord] -> Int
points group
    | p == 3 = 10
    | p == 4 = 20
    | p == 5 = 50
    | p == 6 = 100
    | p >= 7 = 500
    | otherwise = 0
  where
    p = length group

-- Soma total de pontos do tabuleiro atual
totalPoints :: Board -> Int
totalPoints board =
    sum (map points (detectGroups board))