module MyCodeGen
  ( compileSource
  , compileAST
  ) where

import Prelude hiding (lookup)
import Data.List (lookup)
import MyParser (parseMyLang, Stmt(..), Expr(..))
import Sprockell.BasicFunctions
import Sprockell.HardwareTypes

-- Variable metadata: either a scalar or a vector

data VarInfo
  = VarScalar Int           -- single slot
  | VarVector Int Int       -- base slot, length

-- A scope maps names to VarInfo; an environment is a stack of scopes

type Scope = [(String, VarInfo)]
type Env   = [Scope]
type Slot  = Int

-- Top-level API: pure functions that error on failure

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

-- Lookup functions that respect VarInfo

lookupScalar :: String -> Env -> Int
lookupScalar name [] = error $ "Variable not found or not scalar: " ++ name
lookupScalar name (scope:rest) =
  case lookup name scope of
    Just (VarScalar s)      -> s
    Just (VarVector _ _)    -> error $ "Expected scalar but found vector: " ++ name
    Nothing                 -> lookupScalar name rest

lookupVector :: String -> Env -> (Int, Int)
lookupVector name [] = error $ "Variable not found or not vector: " ++ name
lookupVector name (scope:rest) =
  case lookup name scope of
    Just (VarVector base len) -> (base, len)
    Just (VarScalar _)        -> error $ "Expected vector but found scalar: " ++ name
    Nothing                   -> lookupVector name rest

-- Main code generator: fold over statements

genStmt :: (Env, Slot, [Instruction]) -> Stmt -> (Env, Slot, [Instruction])
genStmt (env, nxt, code) stmt = case stmt of
  -- Scalar declaration
  Decl _ name
    | name `elem` map fst (head env)
    -> error $ "Variable already declared in this scope: " ++ name
    | otherwise
    -> let slot     = nxt
           instrs   = [ Load (ImmValue 0) regA
                      , Store regA (DirAddr slot)
                      ]
           newScope = (name, VarScalar slot) : head env
       in ( newScope : tail env
          , slot + 1
          , code ++ instrs
          )

  -- Vector literal declaration: "science v = [e1,e2,...]"
  DeclVec name elems
    | name `elem` map fst (head env)
    -> error $ "Variable already declared in this scope: " ++ name
    | otherwise
    -> let len       = length elems
           baseSlot  = nxt
           -- compile and store each element into successive slots
           storeCodes = concat
             [ compileExpr env regA e
             ++ [ Store regA (DirAddr (baseSlot + i)) ]
             | (i,e) <- zip [0..] elems
             ]
           newScope = (name, VarVector baseSlot len) : head env
       in ( newScope : tail env
          , baseSlot + len
          , code ++ storeCodes
          )

  -- Assignment to scalar
  Assign name expr ->
    let slot     = lookupScalar name env
        exprCode = compileExpr env regA expr
    in (env, nxt, code ++ exprCode ++ [Store regA (DirAddr slot)])

  -- Print expression
  Print expr ->
    let exprCode = compileExpr env regA expr
    in (env, nxt, code ++ exprCode ++ [WriteInstr regA (DirAddr 0x10000)])

  -- Block: new nested scope, popped after
  Block ss ->
    let (_:outerEnv, nxt', bodyCode) = foldl genStmt ( []:env, nxt, [] ) ss
    in ( outerEnv, nxt', code ++ bodyCode )

  -- Conditional (major/having)
  If cond thn mEls ->
    let condC    = compileExpr env regA cond
        -- then-branch
        (_, nTh, thC) = foldl genStmt ( []:env, nxt, [] ) thn
        -- else-branch
        (_, nEl, elC) = case mEls of
          Just els -> foldl genStmt ( []:env, nxt, [] ) els
          Nothing  -> ( []:env, nxt, [] )
        nextSlot = max nTh nEl
        thnLen   = length thC
        elsLen   = length elC
        brCode   = condC
                 ++ [ Branch regA (Rel 2)
                    , Jump  (Rel (thnLen + 2))
                    ]
                 ++ thC
                 ++ [ Jump (Rel (elsLen + 1))
                    ]
                 ++ elC
    in ( env, nextSlot, code ++ brCode )

  -- While loop
  While cond body ->
    let condC        = compileExpr env regA cond
        (_, nxtBd, bdC) = foldl genStmt ( []:env, nxt, [] ) body
        cLen         = length condC
        bLen         = length bdC
        loopCode     = condC
                    ++ [ Branch regA (Rel 2)
                       , Jump (Rel (bLen + 2))
                       ]
                    ++ bdC
                    ++ [ Jump (Rel (-(cLen + bLen + 2))) ]
    in ( env, nxtBd, code ++ loopCode )

  -- ForkJoin and Lock reuse block semantics
  ForkJoin ss ->
    let (_:outerEnv, nxt', fjC) = foldl genStmt ( []:env, nxt, [] ) ss
    in ( outerEnv, nxt', code ++ fjC )

  Lock _ ss ->
    let (_:outerEnv, nxt', lkC) = foldl genStmt ( []:env, nxt, [] ) ss
    in ( outerEnv, nxt', code ++ lkC )

-- Expression compiler

type Reg = Int
compileExpr :: Env -> Reg -> Expr -> [Instruction]
compileExpr env tgt expr = case expr of
  IntLit n   -> [ Load (ImmValue (fromIntegral n)) tgt ]
  BoolLit b  -> [ Load (ImmValue (if b then 1 else 0)) tgt ]
  Var x      -> [ Load (DirAddr (lookupScalar x env)) tgt ]
  ArrayRef name idx ->
    let (base, _) = lookupVector name env
        idxC  = compileExpr env regB idx
        baseC = [ Load (ImmValue base) regC ]
        addrC = [ Compute Add regC regB regC ]
        loadC = [ Load (IndAddr regC) tgt ]

    in idxC ++ baseC ++ addrC ++ loadC
  BinOp op l r ->
    let lC = compileExpr env tgt l
        rC = compileExpr env regB r
    in lC ++ rC ++ [ Compute (toOp op) tgt regB tgt ]
  UnOp "!" e -> compileExpr env tgt e ++ [ Compute Xor tgt tgt tgt ]
  _          -> error $ "Unsupported Expr: " ++ show expr

-- Map string to Operator

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
