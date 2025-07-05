module Main where

import MyParser
import MyCodeGen
import Sprockell

showLocalMem :: DbgInput -> String
showLocalMem ( _ , systemState ) = show $ localMem $ head $ sprStates systemState

doesLocalMemWrite :: DbgInput -> Bool
doesLocalMemWrite (instrs,st) = any isStoreInstr instrs
    where
        isStoreInstr (Store _ _) = True
        isStoreInstr _           = False

showAllRegisters :: DbgInput -> String
showAllRegisters ( _ , systemState ) = show $ map regbank $ sprStates systemState

runFile :: FilePath -> IO ()
runFile path = do
  src <- readFile path
  case parseMyLang src of
    Left err -> putStrLn ("Error parsing " ++ path ++ ": " ++ err)
    Right ast -> do
      let instrs = compileAST ast
      run [instrs]
      -- runWithDebugger (debuggerSimplePrint showAllRegisters) [instrs]
    
main :: IO ()
main = runFile "funcs.siw"
