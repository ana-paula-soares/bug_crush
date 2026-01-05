module Main where

import System.IO
import Control.Concurrent (threadDelay)

-- Importando nossos módulos
import Types
import Board (generateBoard)
import Render (printBoard)
import GameLogic (swap, findMatches, clearMatches, isValidMove, stepGravity, fixInitialMatches)

-- Loop de Animação de Queda
animateFall :: Board -> IO Board
animateFall board = do
    printBoard board
    threadDelay 100000 -- 0.1s
    (nextBoard, moved) <- stepGravity board
    if moved then animateFall nextBoard else return board

-- Loop de Resolução de Cascata
resolveCascades :: Board -> IO Board
resolveCascades board = do
    let matches = findMatches board
    if null matches
        then return board
        else do
            let cleared = clearMatches board matches
            printBoard cleared
            putStrLn "\n   >>> BOOM! <<<"
            threadDelay 800000 -- 0.8s
            
            stableBoard <- animateFall cleared
            resolveCascades stableBoard

-- Input do Usuário
getUserInput :: IO (Maybe (Coord, Coord))
getUserInput = do
    putStrLn "\nDigite: Linha Coluna Direção (w/s/a/d). 'q' para sair."
    putStr "> "
    hFlush stdout
    line <- getLine
    if line == "q" then return Nothing else
        case words line of
            [r, c, dir] -> do
                let r1 = read r; c1 = read c
                let (r2, c2) = case dir of
                        "w" -> (r1-1, c1); "s" -> (r1+1, c1)
                        "a" -> (r1, c1-1); "d" -> (r1, c1+1)
                        _ -> (r1, c1)
                return $ Just ((r1, c1), (r2, c2))
            _ -> return $ Just ((-1,-1), (-1,-1))

-- Loop Principal
gameLoop :: Board -> IO ()
gameLoop board = do
    printBoard board
    input <- getUserInput
    case input of
        Nothing -> putStrLn "Fim de jogo!"
        Just (c1, c2) -> do
            if not (isValidMove c1 c2) 
                then gameLoop board
                else do
                    let swapped = swap board c1 c2
                    printBoard swapped
                    putStrLn "\n   >>> TROCANDO... <<<"
                    threadDelay 1500000 -- 1.5s
                    
                    if null (findMatches swapped)
                        then do
                            putStrLn "Inválido! Voltando..."
                            threadDelay 1000000
                            gameLoop board
                        else do
                            finalBoard <- resolveCascades swapped
                            gameLoop finalBoard

-- Entrada do Programa
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Iniciando Haskell Crush..."
    
    -- Gera e conserta o tabuleiro inicial
    rawBoard <- generateBoard
    cleanBoard <- fixInitialMatches rawBoard
    
    gameLoop cleanBoard