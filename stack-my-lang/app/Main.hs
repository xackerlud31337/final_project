module Main where

import MyParser (parseMyLang)
import MyCodeGen (codeGen)
import Sprockell (run, Instruction)

-- Compiles a number into a spril program producing all fibonacci numbers below the number
-- Compilation might fail
compile :: String -> Either String [Instruction]
compile txt = do
    ast <- parseMyLang txt
    pure $ codeGen ast

-- Gets a number and runs the resulting spril program of compilation succeeds
main :: IO ()
main = do
    txt <- getLine
    case compile txt of
        (Left err) -> fail err
        (Right spril) -> run [spril]
