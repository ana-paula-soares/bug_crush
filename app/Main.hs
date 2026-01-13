module Main where

import System.IO
import Control.Concurrent (threadDelay)

-- Importando nossos módulos
import Types
import Board (generateBoard)
import Render (printBoard)
import GameLogic (swap, findMatches, clearMatches, isValidMove, stepGravity, fixInitialMatches, points, detectGroups)
import UI (telaInicial,menuInicial,telaRegras,telaInstrucoes,telaLogin,renderHUD,limparTela,telaGameOver)

-- Loop de Animação de Queda
animateFall :: Board -> IO Board
animateFall board = do
    printBoard board
    threadDelay 100000 -- 0.1s
    (nextBoard, moved) <- stepGravity board
    if moved then animateFall nextBoard else return board

-- Loop de Resolução de Cascata
resolveCascades :: Board -> Int -> IO (Board, Int)
resolveCascades board comboMultiplier = do
    let groups = detectGroups board

    if null groups
        then return (board, 0)
        else do
            let -- 1. Pontuação base: soma simples dos grupos atuais
                basePts = sum (map points groups)
                
                -- 2. O bónus só existe se comboMultiplier > 0 (ou seja, se já houve uma queda)
                multiplicador = if comboMultiplier == 0 
                                then 1.0 
                                else 1.0 + (0.5 * fromIntegral comboMultiplier)
                
                totalDaRodada = round (fromIntegral basePts * multiplicador)
                
                -- Limpa todas as peças que deram match
                allCoords = concat groups
                cleared = clearMatches board allCoords

            printBoard cleared
            
            -- Feedback visual diferenciado
            if comboMultiplier == 0
                then putStrLn $ "\n   >>> EXPLOSÃO! +" ++ show totalDaRodada ++ " pts <<<"
                else putStrLn $ "\n   >>> CASCATA COMBO x" ++ show (comboMultiplier + 1) ++ "! +" ++ show totalDaRodada ++ " pts <<<"
            
            threadDelay 800000 
            
            -- 3. ANTES de aumentar o multiplicador, as peças CAEM
            stableBoard <- animateFall cleared
            
            -- 4. Agora sim, chamamos recursivamente com multiplicador + 1
            (finalBoard, pontosDasCascatas) <- resolveCascades stableBoard (comboMultiplier + 1)
            
            return (finalBoard, totalDaRodada + pontosDasCascatas)

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
--Precisei refatorar para ser possível exibir elementos da interface
--O loop principal do jogo agora recebe:
--  nome -> nome do jogador(vindo da tela de login)
--  pontos -> pontuação atual do jogador
--  movimentos -> movimentos restantes
--  board -> estado atual do tabuleiro
--A pontuação é mantida como parâmetro explícito com valores temporários
--aguardando a finalização da lógica de cálculo
gameLoop :: String -> Int -> Int -> Board -> IO () 
gameLoop nome pontos movimentos board = do
    --CONDIÇÂO DE FIM DE JOGO(temporário, se não quiserem basta remover a condição)
    --Quando os movimentos acabam, encerro o loop
    if movimentos <= 0
        then telaGameOver nome pontos
        else do
            limparTela --limpa a tela antes de redesenhar HUD e tabuleiro
            renderHUD nome pontos movimentos--exibe HUD com nome do jogador e pontuação atual
            printBoard board
            input <- getUserInput
            case input of
                Nothing -> telaGameOver nome pontos >> loopMenu --Achei interessante que depois do fim do jogo, retornasse a tela de Menu Inicial
                Just (c1, c2) -> do
                    if not (isValidMove c1 c2) 
                        then gameLoop nome pontos movimentos board
                        else do
                            let swapped = swap board c1 c2
                            printBoard swapped
                            putStrLn "\n   >>> TROCANDO... <<<"
                            threadDelay 1500000 -- 1.5s
                    
                            if null (findMatches swapped)
                                then do
                                    putStrLn "Inválido! Voltando..."
                                    threadDelay 1000000
                                    gameLoop nome pontos movimentos board
                                else do
                                    (finalBoard, finalPoints) <- resolveCascades swapped 0
                            
                                    --PLACEHOLDER DA PONTUAÇÃO:
                                    --No momentos os pontos não são calculados
                                    let novosPontos = pontos + finalPoints
                            
                                    --PLACEHOLDER DE MOVIMENTOS:
                                    --Cada jogada válida consome exatamente 1 movimento
                                    --Essa regra pode ser alterada conforme a lógica final do jogo
                                    let novosMovimentos = movimentos - 1

                                    --Continua o jogo com o novo tabuleiro
                                    gameLoop nome novosPontos novosMovimentos finalBoard

--Inicia uma nova partida
--A pontuação começa em 0 e será atualizada conforme a lógica de contagem for integrada
--A quantidade de movimentos também começa com valores fixos (por enquanto não sabemos o valor exato)
iniciarJogo :: String -> IO ()
iniciarJogo nome = do
    --Gera e conserta o tabuleira inicial
    rawBoard <- generateBoard
    cleanBoard <- fixInitialMatches rawBoard
    let pontosIniciais = 0 --Pontuação inicial do jogador
    let movimentosIniciais = 30 --Valor arbitrário
    gameLoop nome pontosIniciais movimentosIniciais cleanBoard

-- Entrada do Programa
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    telaInicial
    loopMenu

loopMenu :: IO ()
loopMenu = do
    opcao <- menuInicial
    case opcao of
        1 -> do
            nome <- telaLogin
            iniciarJogo nome
        2 -> telaRegras  >> loopMenu
        3 -> telaInstrucoes >> loopMenu
        4 -> putStrLn "Saindo do jogo..."
        _ -> loopMenu