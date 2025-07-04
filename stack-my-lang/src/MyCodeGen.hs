module MyCodeGen
  ( compileSource
  , compileAST
  ) where

import Prelude hiding (lookup)
import MyParser (parseMyLang, Stmt(..), Expr(..))
import Sprockell.BasicFunctions (regA, regB, regC, regD, regE, regF)
import Sprockell.HardwareTypes

type Env  = [(String, Int)]

type Slot = Int

compileSource :: String -> Either String [Instruction]
compileSource src =
  case parseMyLang src of
    Left err  -> Left err
    Right ast -> Right (compileAST ast)

compileAST :: [Stmt] -> [Instruction]
compileAST stmts =
  let (_, _, code) = foldl genStmt ([], 0, []) stmts
  in code ++ [EndProg]

genStmt :: (Env, Slot, [Instruction]) -> Stmt -> (Env, Slot, [Instruction])
genStmt (env, nxt, code) stmt = case stmt of
  Decl _ name ->
    let slot   = nxt
        instrs = [ Load (ImmValue 0) regA
                 , Store regA (DirAddr slot)
                 ]
    in ((name,slot):env, slot+1, code ++ instrs)

  Assign name expr ->
    let slot     = lookup name env
        exprCode = compileExpr env regA expr
    in (env, nxt, code ++ exprCode ++ [Store regA (DirAddr slot)])

  Print expr ->
    let exprCode = compileExpr env regA expr
    in (env, nxt, code ++ exprCode ++ [WriteInstr regA (DirAddr 0x10000)])

  Block ss ->
    let (env', nxt', code') = foldl genStmt (env, nxt, []) ss
    in (env, nxt', code ++ code')

  If cond thn mEls ->
    let condC       = compileExpr env regA cond
        (_, nxt1, thnC) = foldl genStmt (env, nxt, []) thn
        (_, nxt2, elsC) = maybe (env, nxt1, []) (foldl genStmt (env, nxt1, [])) mEls
        thnLen      = length thnC
        elsLen      = length elsC
        offThen     = thnLen + 2
        offEnd      = elsLen  + 1
    in (env, max nxt1 nxt2, code
         ++ condC
         ++ [Branch regA (Rel offThen)]
         ++ thnC
         ++ [Jump (Rel offEnd)]
         ++ elsC)

  While cond body ->
    let condC       = compileExpr env regA cond
        (_, nxt', bdyC) = foldl genStmt (env, nxt, []) body
        condLen     = length condC + 1
        bodyLen     = length bdyC + 1
        offSkip     = condLen
        offBack     = -(condLen + bodyLen)
    in (env, nxt', code
         ++ condC
         ++ [Branch regA (Rel offSkip)]
         ++ bdyC
         ++ [Jump (Rel offBack)])

  ForkJoin ss ->
    let (_, nxt', fC) = foldl genStmt (env, nxt, []) ss
    in (env, nxt', code ++ fC)

  Lock _ ss ->
    let (_, nxt', lC) = foldl genStmt (env, nxt, []) ss
    in (env, nxt', code ++ lC)

type Reg = Int
compileExpr :: Env -> Reg -> Expr -> [Instruction]
compileExpr env tgt expr = case expr of
  IntLit n     -> [Load (ImmValue (fromIntegral n)) tgt]
  BoolLit b    -> [Load (ImmValue (if b then 1 else 0)) tgt]
  Var x        -> [Load (DirAddr (lookup x env)) tgt]
  BinOp op l r ->
    let lC = compileExpr env tgt l
        rC = compileExpr env regB r
    in lC ++ rC ++ [Compute (toOp op) tgt regB tgt]
  UnOp "!" e  -> compileExpr env tgt e ++ [Compute Xor tgt tgt tgt]
  _            -> error $ "Unsupported Expr: " ++ show expr


toOp :: String -> Operator
toOp "+"  = Add
toOp "-"  = Sub
toOp "*"  = Mul
toOp "==" = Equal
toOp "!=" = NEq
toOp "<"  = Lt
toOp "<=" = LtE
toOp ">"  = Gt
toOp ">=" = GtE
toOp "&&" = And
toOp "||" = Or
toOp _     = error "Unknown operator"

lookup :: String -> Env -> Int
lookup k env = case [s | (x,s) <- env, x == k] of
  (s:_) -> s
  []    -> error $ "Variable not found: " ++ k
