module Main where

import MyParser
import MyCodeGen
import Sprockell
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Control.Monad (when)

showLocalMem :: DbgInput -> String
showLocalMem ( _ , systemState ) = show $ localMem $ head $ sprStates systemState

doesLocalMemWrite :: DbgInput -> Bool
doesLocalMemWrite (instrs,st) = any isStoreInstr instrs
    where
        isStoreInstr (Store _ _) = True
        isStoreInstr _           = False

showAllRegisters :: DbgInput -> String
showAllRegisters ( _ , systemState ) = show $ map regbank $ sprStates systemState

showSharedMem :: DbgInput -> String
showSharedMem ( _ , systemState ) = show $ sharedMem systemState

runFile :: FilePath -> IO ()
runFile path = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  
  src <- readFile path
  case parseMyLang src of
    Left err -> putStrLn ("Error parsing " ++ path ++ ": " ++ err)
    Right ast -> do
      let progs = compileSource src
      
      -- Show compilation info
      putStrLn $ "\nCompiled " ++ path ++ " into " ++ show (length progs) ++ " Sprockell programs"
      
      -- Show first few instructions for debugging
      mapM_ (\(i, prog) -> do
        putStrLn $ "\nSprockell " ++ show i ++ " (" ++ show (length prog) ++ " instructions):"
        mapM_ (\(j, instr) -> putStrLn $ "  " ++ show j ++ ": " ++ show instr) (zip [0..] (take 15 prog))
        when (length prog > 15) $ putStrLn "  ..."
        ) (zip [0..] progs)
      
      putStrLn "\nRunning program..."
      run progs

main :: IO ()
main = do
  putStrLn "Enter the name of the file to run:"
  fileName <- getLine
  runFile fileName