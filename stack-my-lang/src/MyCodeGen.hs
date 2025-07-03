module MyCodeGen
    ( codeGen ) where

import Sprockell (Instruction(..))
import MyParser (Stmt(..), Expr(..))
import MyCompiler


-- -- Entry point
-- codeGen :: String -> [Instruction]
-- codeGen stmts = prettyInstructions (compileString "i x; x = 2*3+1; print x;")

-- -- Compile a statement into Sprockell instructions
-- compileStmt :: Stmt -> [Instruction]
-- compileStmt stmt = case stmt of
--     Print e       -> compileExpr e ++ [WriteInstr regA numberIO]
--     Assign _ _    -> [] -- placeholder
--     Decl _ _      -> [] -- placeholder
--     If _ _ _      -> [] -- not implemented yet
--     While _ _     -> [] -- not implemented yet
--     ForkJoin _    -> [] -- not implemented yet
--     Lock _ _      -> [] -- not implemented yet
--     Block ss      -> concatMap compileStmt ss

-- -- Compile an expression into Sprockell instructions
-- -- Result will be in regA
-- compileExpr :: Expr -> [Instruction]
-- compileExpr expr = case expr of
--     IntLit n        -> [Load (ImmValue $ fromInteger n) regA]
--     BinOp "+" l r   -> compileBinary l r Add
--     BinOp "-" l r   -> compileBinary l r Sub
--     BinOp "*" l r   -> compileBinary l r Mul
--     BinOp "/" l r   -> compileBinary l r Div
--     _               -> [NOP] -- unsupported expr



prog :: String -> [Instruction]
prog x = compileString x


-- prog :: [Instruction]
-- prog = [ Load (ImmValue 2) regA
--         , Load (ImmValue 3) regB
--         , Compute Mul regA regB regA
--         , Load (ImmValue 1) regB
--         , Compute Add regA regB regA
--         , WriteInstr regA numberIO
--         , EndProg
--        ]

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