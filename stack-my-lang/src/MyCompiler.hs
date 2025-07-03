module MyCompiler
  ( compileString
  ) where

import MyParser   (parseMyLang, Expr(..), Stmt(..))
import Data.Char  (ord)
import Sprockell  ( Instruction(..)
                 , Operator(..)
                 , AddrImmDI(..)
                 , Target(..)
                 , numberIO
                 , regA, regB, regC, regD, regE
                 )

-- our scratch-register pool
regPool :: [Int]
regPool = [regA, regB, regC, regD, regE]

-- entry point: parse & compile, then EndProg
compileString :: String -> [Instruction]
compileString src =
  case parseMyLang src of
    Left err    -> error err
    Right stmts ->
      let (code, _, _) = compileStmts 0 regPool stmts
      in code ++ [EndProg]

-- compile a list of stmts, threading a label counter and available regs
compileStmts :: Int -> [Int] -> [Stmt] -> ([Instruction], [Int], Int)
compileStmts lbl regs = foldl go ([], regs, lbl)
  where
    go (acc, rs, l) stmt =
      let (c, rs', l') = compileStmt l rs stmt
      in (acc ++ c, rs', l')

-- compile one statement
compileStmt :: Int -> [Int] -> Stmt -> ([Instruction], [Int], Int)
compileStmt lbl regs stmt = case stmt of

  Decl _ _ ->
    -- no runtime code for declarations
    ([], regs, lbl)

  Assign v e ->
    -- compute e into r and keep it live (no Store)
    let (r, cE, rs') = compileExpr regs e
    in (cE, r:rs', lbl)

  Print (Var _) ->
    -- variable already in register: just write it out
    let r = head regs
    in ([ WriteInstr r numberIO ], regs, lbl)

  Print e ->
    -- evaluate e into r, then write it out
    let (r, cE, rs') = compileExpr regs e
    in (cE ++ [ WriteInstr r numberIO ], rs', lbl)

  Block ss ->
    compileStmts lbl regs ss

  If cond thenSs maybeElseSs ->
    let (rCond, cCond, regs1) = compileExpr regs cond
        lblThen = lbl + 1
        lblElse = lbl + 2
        thenCode = fst3 $ compileStmts lblThen regs1 thenSs
        elseCode = maybe [] (fst3 . compileStmts lblElse regs1) maybeElseSs
        lenThen = length thenCode + 1
        lenElse = length elseCode
        cIf =  cCond
            ++ [ Branch rCond (Rel 2)
               , Jump   (Rel (lenThen + 1))
               ]
            ++ thenCode
            ++ [ Jump (Rel lenElse) ]
            ++ elseCode
    in (cIf, regs1, lbl + 2)

  _ ->
    error $ "Unsupported statement: " ++ show stmt
  where
    fst3 (x,_,_) = x

--------------------------------------------------------------------------------
-- expression → (target-reg, prepended code, remaining regs)
--------------------------------------------------------------------------------

compileExpr :: [Int] -> Expr -> (Int, [Instruction], [Int])
compileExpr (r:rs) (IntLit n) =
  (r, [ Load (ImmValue (fromInteger n)) r ], rs)

compileExpr (r:rs) (BoolLit b) =
  (r, [ Load (ImmValue (if b then 1 else 0)) r ], rs)

compileExpr (r:rs) (Var v) =
  (r, [ Load (DirAddr (varToAddr v)) r ], rs)

compileExpr regs@(r:_) (UnOp op e) =
  let (r1, c1, rs1) = compileExpr regs e
  in (r1, c1 ++ [ Compute (toOperator op) r1 r1 r1 ], rs1)

compileExpr regs expr@(BinOp op l r) =
  let dl = exprDepth l
      dr = exprDepth r
      (big, small) = if dl >= dr then (l,r) else (r,l)
      (rBig, cBig, rsBig) = compileExpr regs big
      (rSml, cSml, rsSml) = compileExpr rsBig small
      cOp = [ Compute (toOperator op) rBig rSml rBig ]
  in (rBig, cBig ++ cSml ++ cOp, rsSml)

compileExpr _ e =
  error $ "compileExpr: unsupported " ++ show e

--------------------------------------------------------------------------------
-- operator mapping
--------------------------------------------------------------------------------

toOperator :: String -> Operator
toOperator "+"  = Add
toOperator "-"  = Sub
toOperator "*"  = Mul
toOperator "==" = Equal
toOperator "/=" = NEq
toOperator ">"  = Gt
toOperator ">=" = GtE
toOperator "<"  = Lt
toOperator "<=" = LtE
toOperator o      = error $ "Unknown op: " ++ o

--------------------------------------------------------------------------------
-- tree depth heuristic for regs
--------------------------------------------------------------------------------

exprDepth :: Expr -> Int
exprDepth (IntLit _)    = 1
exprDepth (BoolLit _)   = 1
exprDepth (Var   _)     = 1
exprDepth (UnOp _ e)    = 1 + exprDepth e
exprDepth (BinOp _ x y) = 1 + max (exprDepth x) (exprDepth y)

--------------------------------------------------------------------------------
-- placeholder variable-address mapping
--------------------------------------------------------------------------------

varToAddr :: String -> Int
varToAddr _ = 10
