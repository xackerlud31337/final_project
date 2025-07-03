module Main where

import MyParser (parseMyLang)
import MyCodeGen (codeGen, prog)
import Sprockell (run, Instruction)

-- Compiles a number into a spril program producing all fibonacci numbers below the number
-- Compilation might fail
compile :: String -> Either String [Instruction]
case parseMyLang src of
    Left err    -> error err
    Right stmts -> let (instrs, _, _) = compileStmtsWithLabels regPool 0 stmts in instrs ++ [EndProg]

-- Gets a number and runs the resulting spril program of compilation succeeds
main :: IO ()
main = run [prog]
