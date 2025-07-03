module MyCodeGen
    ( codeGen ) where

import Sprockell
import MyParser (Stmt(..), Expr(..))

-- This is the correct type for integration with Main.hs
codeGen :: [Stmt] -> [Instruction]
codeGen stmts = concatMap compileStmt stmts ++ [EndProg]

-- Dummy implementation: just handle printing ints
compileStmt :: Stmt -> [Instruction]
compileStmt (Print (IntLit n)) =
    [ Load (ImmValue $ fromInteger n) regA
    , WriteInstr regA numberIO
    ]
compileStmt _ = []  -- other statements not implemented yet