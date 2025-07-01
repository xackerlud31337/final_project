module MyCompilerPure
  ( compileString  
  ) where

import MyParser   (parseMyLang, Expr(..), Stmt(..))

-- | Virtual machine instructions
data Instr
    = LoadConst Integer Reg
    | LoadVar   String  Reg
    | BinInstr  String Reg Reg Reg  -- op left right dest
    | Store     Reg    String
    | PrintReg  Reg
    deriving (Show)

-- | Exactly five hardware registers
data Reg = RegA | RegB | RegC | RegD | RegE
    deriving (Show, Eq)

-- | Initial pool of all registers
regPool :: [Reg]
regPool = [RegA, RegB, RegC, RegD, RegE]

--------------------------------------------------------------------------------
-- Top-level API
--------------------------------------------------------------------------------

-- | Parse & compile a program string to [Instr]
compileString :: String -> [Instr]
compileString src =
  case parseMyLang src of
    Left err    -> error err
    Right stmts -> fst $ compileStmts regPool stmts

--------------------------------------------------------------------------------
-- Statement compilation (pure)
--------------------------------------------------------------------------------

-- | Compile a list of statements, threading the register pool
compileStmts :: [Reg] -> [Stmt] -> ([Instr], [Reg])
compileStmts regs = foldl step ([], regs)
  where
    step (accCode, accRegs) stmt =
      let (codeS, regs') = compileStmt accRegs stmt
      in (accCode ++ codeS, regs')

-- | Compile a single statement, returning generated code and updated register pool
compileStmt :: [Reg] -> Stmt -> ([Instr], [Reg])
compileStmt regs stmt =
  case stmt of
    Decl _ _ -> ([], regs)

    Assign v e ->
      let (r, codeE, regs') = compileExpr regs e
          codeStore        = [Store r v]
          regsOut          = r : regs'
      in (codeE ++ codeStore, regsOut)

    Print e ->
      let (r, codeE, regs') = compileExpr regs e
          codePrint        = [PrintReg r]
          regsOut          = r : regs'
      in (codeE ++ codePrint, regsOut)

    Block ss -> compileStmts regs ss

    _ -> ([], regs)

--------------------------------------------------------------------------------
-- Expression compilation (pure, no monads)
--------------------------------------------------------------------------------

-- | Compile an expression with given register pool,
--   returning (allocated register, generated code, remaining pool)
compileExpr :: [Reg] -> Expr -> (Reg, [Instr], [Reg])

compileExpr (r:rs) (IntLit n) =
  (r, [LoadConst n r], rs)

compileExpr (r:rs) (Var v) =
  (r, [LoadVar v r], rs)

compileExpr regs@(r:_) (UnOp op e) =
  let (r1, code1, regs1) = compileExpr regs e
      code2 = code1 ++ [BinInstr op r1 r1 r1]
  in (r1, code2, regs1)

compileExpr regs expr@(BinOp op l r) =
  -- compute deeper side first (minimize live regs)
  let dl = exprDepth l
      dr = exprDepth r
      (le, re) = if dl >= dr then (l, r) else (r, l)
      -- compile left (deeper) then right
      (rL, codeL, regsL) = compileExpr regs le
      (rR, codeR, regsR) = compileExpr regsL re
      codeOp = [BinInstr op rL rR rL]
      regsOut = rR : regsR  -- free rR
  in (rL, codeL ++ codeR ++ codeOp, regsOut)

compileExpr _ _ = error "Unsupported expression in compileExpr"

-- | Compute expression tree depth
exprDepth :: Expr -> Int
exprDepth (IntLit _)    = 1
exprDepth (BoolLit _)   = 1
exprDepth (Var   _)     = 1
exprDepth (UnOp _ e)    = 1 + exprDepth e
exprDepth (BinOp _ x y) = 1 + max (exprDepth x) (exprDepth y)
