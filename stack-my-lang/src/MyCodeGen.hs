module MyCodeGen
  ( compileSource
  , compileAST
  ) where

import Prelude hiding (lookup)
import MyParser (parseMyLang, Stmt(..), Expr(..))
import Sprockell.BasicFunctions (regA, regB)
import Sprockell.HardwareTypes

type Scope = [(String, Int)]
type Env   = [Scope]
type Slot  = Int

compileSource :: String -> [Instruction]
compileSource src =
  case parseMyLang src of
    Left err  -> error err
    Right ast -> compileAST ast

compileAST :: [Stmt] -> [Instruction]
compileAST stmts =
  let initialEnv  = [ [] ]
      initialSlot = 0
      (_, _, code) = foldl genStmt (initialEnv, initialSlot, []) stmts
  in code ++ [EndProg]

genStmt :: (Env, Slot, [Instruction]) -> Stmt -> (Env, Slot, [Instruction])
genStmt (env, nxt, code) stmt = case stmt of
  Decl _ name
    | name `elem` map fst (head env)
    -> error $ "Variable already declared in this scope: " ++ name
    | otherwise
    -> let slot   = nxt
           instrs = [ Load (ImmValue 0) regA
                    , Store regA (DirAddr slot)
                    ]
           newScope = (name, slot) : head env
       in ( newScope : tail env
          , slot + 1
          , code ++ instrs
          )

  Assign name expr ->
    let slot     = lookup name env
        exprCode = compileExpr env regA expr
    in (env, nxt, code ++ exprCode ++ [Store regA (DirAddr slot)])

  Print expr ->
    let exprCode = compileExpr env regA expr
    in (env, nxt, code ++ exprCode ++ [WriteInstr regA (DirAddr 0x10000)])

  Block ss ->
    let ( (_:outer), nxt', code') = foldl genStmt ( []:env, nxt, [] ) ss
    in ( outer, nxt', code ++ code' )

  If cond thn mEls ->
    let condC = compileExpr env regA cond
        (_, nxtTh, thC) = foldl genStmt ( []:env, nxt, [] ) thn
        ( _    , nxtEl, elC) = case mEls of
          Just els -> foldl genStmt ( []:env, nxt, [] ) els
          Nothing  -> ( []:env, nxt, [] )
        nextSlot = max nxtTh nxtEl
        thenLen  = length thC
        elseLen  = length elC
        offThen  = thenLen + 2
        offEnd   = elseLen + 1
        branchCode = condC
                  ++ [ Branch regA (Rel offThen) ]
                  ++ thC
                  ++ [ Jump (Rel offEnd) ]
                  ++ elC
    in ( env, nextSlot, code ++ branchCode )

  While cond body ->
    let condC        = compileExpr env regA cond
        (_, nxtBd, bdC) = foldl genStmt ( []:env, nxt, [] ) body
        cLen          = length condC
        bLen          = length bdC
        loopCode = condC
                 ++ [ Branch regA (Rel 2)
                    , Jump (Rel (bLen + 2))
                    ]
                 ++ bdC
                 ++ [ Jump (Rel (-(cLen + bLen + 2))) ]
    in ( env, nxtBd, code ++ loopCode )

  ForkJoin ss ->
    let ( (_:outer), nxt', code') = foldl genStmt ( []:env, nxt, [] ) ss
    in ( outer, nxt', code ++ code' )

  Lock _ ss ->
    let ( (_:outer), nxt', code') = foldl genStmt ( []:env, nxt, [] ) ss
    in ( outer, nxt', code ++ code' )

type Reg = Int
compileExpr :: Env -> Reg -> Expr -> [Instruction]
compileExpr env tgt expr = case expr of
  IntLit n     -> [ Load (ImmValue (fromIntegral n)) tgt ]
  BoolLit b    -> [ Load (ImmValue (if b then 1 else 0)) tgt ]
  Var x        -> [ Load (DirAddr (lookup x env)) tgt ]
  BinOp op l r ->
    let lC = compileExpr env tgt l
        rC = compileExpr env regB r
    in lC ++ rC ++ [ Compute (toOp op) tgt regB tgt ]
  UnOp "!" e  -> compileExpr env tgt e ++ [ Compute Xor tgt tgt tgt ]
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
lookup name [] = error $ "Variable not found: " ++ name
lookup name (scope:rest) =
  case [ slot | (n,slot) <- scope, n == name ] of
    (s:_) -> s
    []    -> lookup name rest
