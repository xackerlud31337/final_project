module MyCodeGen
    ( codeGen ) where

import Sprockell
import MyParser (Stmt(..), Expr(..))
import MyCompiler


-- Entry point
codeGen :: [Stmt] -> [Instruction]
codeGen stmts = concatMap compileStmt stmts ++ [EndProg]

-- Compile a statement into Sprockell instructions
compileStmt :: Stmt -> [Instruction]
compileStmt stmt = case stmt of
    Print e       -> compileExpr e ++ [WriteInstr regA numberIO]
    Assign _ _    -> [] -- placeholder
    Decl _ _      -> [] -- placeholder
    If _ _ _      -> [] -- not implemented yet
    While _ _     -> [] -- not implemented yet
    ForkJoin _    -> [] -- not implemented yet
    Lock _ _      -> [] -- not implemented yet
    Block ss      -> concatMap compileStmt ss

-- Compile an expression into Sprockell instructions
-- Result will be in regA
compileExpr :: Expr -> [Instruction]
compileExpr expr = case expr of
    IntLit n        -> [Load (ImmValue $ fromInteger n) regA]
    BinOp "+" l r   -> compileBinary l r Add
    BinOp "-" l r   -> compileBinary l r Sub
    BinOp "*" l r   -> compileBinary l r Mul
    BinOp "/" l r   -> compileBinary l r Div
    _               -> [NOP] -- unsupported expr