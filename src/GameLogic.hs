module GameLogic where

import Types
import Board (get, set, randomCandy, replaceList) -- Importa ferramentas
import Data.List (nub)

-- 1. Regras Puras

swap :: Board -> Coord -> Coord -> Board
swap board c1 c2 =
    let val1 = get board c1
        val2 = get board c2
    in set (set board c1 val2) c2 val1

isValidMove :: Coord -> Coord -> Bool
isValidMove (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2) == 1

findMatches :: Board -> [Coord]
findMatches board = nub (horiz ++ vert)
  where
    horiz = [ (r, c+k) | r <- [0..height-1], c <- [0..width-3],
              let k1 = get board (r,c), let k2 = get board (r,c+1), let k3 = get board (r,c+2),
              k1 /= Empty, k1 == k2, k2 == k3, k <- [0,1,2] ]
    vert  = [ (r+k, c) | r <- [0..height-3], c <- [0..width-1],
              let k1 = get board (r,c), let k2 = get board (r+1,c), let k3 = get board (r+2,c),
              k1 /= Empty, k1 == k2, k2 == k3, k <- [0,1,2] ]

clearMatches :: Board -> [Coord] -> Board
clearMatches board [] = board
clearMatches board (c:cs) = clearMatches (set board c Empty) cs

-- 2. Física (Envolve IO pois gera doces aleatórios)

stepGravity :: Board -> IO (Board, Bool)
stepGravity board = do
    let topRow = board !! 0
    newTopRow <- mapM (\c -> if c == Empty then randomCandy else return c) topRow
    let boardWithTop = replaceList 0 newTopRow board
    
    let (finalBoard, moved) = foldl processRow (boardWithTop, False) [height-1, height-2 .. 1]
    return (finalBoard, moved || (topRow /= newTopRow))
  where
    processRow (b, changed) r = 
        foldl (\(currB, currChg) c -> 
            let me = get currB (r, c)
                up = get currB (r-1, c)
            in if me == Empty && up /= Empty
               then (set (set currB (r, c) up) (r-1, c) Empty, True)
               else (currB, currChg)
        ) (b, changed) [0..width-1]

-- 3. Correção Inicial (Necessária para o generateBoard funcionar perfeitamente)
fixInitialMatches :: Board -> IO Board
fixInitialMatches board = do
    let matches = findMatches board
    if null matches
        then return board
        else do
            fixedBoard <- replaceAtCoords board matches
            fixInitialMatches fixedBoard

replaceAtCoords :: Board -> [Coord] -> IO Board
replaceAtCoords board [] = return board
replaceAtCoords board (c:cs) = do
    newC <- randomCandy
    let safeC = if newC == Empty then Red else newC
    let newB = set board c safeC
    replaceAtCoords newB cs