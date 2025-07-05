module MyCodeGen
  ( compileSource
  , compileAST
  ) where

import Prelude hiding (lookup)
import Data.List (lookup)
import Data.Char (ord)
import MyParser (parseMyLang, Stmt(..), Expr(..))
import Sprockell

-- | Element types (for vectors/strings)
data ElemType = ETInt | ETChar deriving (Eq, Show)

-- | Variable metadata: scalar or vector
--   Scalars occupy one slot; vectors occupy consecutive slots with an element type.
data VarInfo
  = VarScalar Int              -- single slot
  | VarVector Int Int ElemType -- base slot, length, element type

-- | A scope maps names to VarInfo; Env is a stack of scopes
type Scope = [(String, VarInfo)]
type Env   = [Scope]
type Slot  = Int

-- | Entry point: compile source text to Sprockell instructions
compileSource :: String -> [Instruction]
compileSource src =
  case parseMyLang src of
    Left err  -> error $ show err
    Right ast -> compileAST ast

compileAST :: [Stmt] -> [Instruction]
compileAST stmts =
  let initialEnv  = [ [] ]
      initialSlot = 0
      (_, _, body) = foldl genStmt (initialEnv, initialSlot, []) stmts
  in body ++ [EndProg]

-- | Lookup a scalar variable slot
lookupScalar :: String -> Env -> Int
lookupScalar name [] = error $ "Variable not found or not scalar: " ++ name
lookupScalar name (scope:rest) =
  case lookup name scope of
    Just (VarScalar s)     -> s
    Just (VarVector _ _ _) -> error $ "Expected scalar but found vector: " ++ name
    Nothing                -> lookupScalar name rest

-- | Lookup a vector: returns (base, length, elemType)
lookupVector :: String -> Env -> (Int, Int, ElemType)
lookupVector name [] = error $ "Variable not found or not vector: " ++ name
lookupVector name (scope:rest) =
  case lookup name scope of
    Just (VarVector b l et) -> (b, l, et)
    Just (VarScalar _)      -> error $ "Expected vector but found scalar: " ++ name
    Nothing                 -> lookupVector name rest

-- | Code generator for statements
genStmt :: (Env, Slot, [Instruction]) -> Stmt -> (Env, Slot, [Instruction])
genStmt (env, nxt, code) stmt = case stmt of
  -- scalar declaration: i x; or am y;
  Decl _ name
    | name `elem` map fst (head env)
    -> error $ "Variable already declared: " ++ name
    | otherwise
    -> let slot     = nxt
           initI    = [ Load (ImmValue 0) regA, Store regA (DirAddr slot) ]
           newScope = (name, VarScalar slot) : head env
       in ( newScope : tail env, slot + 1, code ++ initI )

  -- string literal: sci s = "...";
  DeclStr name s
    | name `elem` map fst (head env)
    -> error $ "Variable already declared: " ++ name
    | otherwise
    -> let len      = length s
           base     = nxt
           initC    = concat
             [ [ Load (ImmValue (ord c)) regA
               , Store regA (DirAddr (base + i)) ]
             | (i,c) <- zip [0..] s ]
           termC    = [ Load (ImmValue 0) regA, Store regA (DirAddr (base + len)) ]
           newScope = (name, VarVector base (len + 1) ETChar) : head env
       in ( newScope : tail env, base + len + 1, code ++ initC ++ termC )

  -- vector literal: sci v = [e1,e2,...];
  DeclVec name elems
    | name `elem` map fst (head env)
    -> error $ "Variable already declared: " ++ name
    | otherwise
    -> let len      = length elems
           base     = nxt
           storeC   = concat [ compileExpr env regA e ++ [ Store regA (DirAddr (base + i)) ]
                             | (i,e) <- zip [0..] elems ]
           newScope = (name, VarVector base len ETInt) : head env
       in ( newScope : tail env, base + len, code ++ storeC )

  -- assignment: x = expr;
  Assign name expr
    -> let slot  = lookupScalar name env
           ecs   = compileExpr env regA expr
       in (env, nxt, code ++ ecs ++ [ Store regA (DirAddr slot) ])

  -- print: print expr;
  Print expr ->
    let ecs = compileExpr env regA expr
        dev = case expr of
          StringLit _  -> charIO
          ArrayRef _ _ -> charIO
          _            -> numberIO
    in  (env, nxt, code ++ ecs ++ [ WriteInstr regA dev ])

  -- block: { ... }
  Block ss
    -> let (_:outer, nxt', bc) = foldl genStmt ( []:env, nxt, [] ) ss
       in (outer, nxt', code ++ bc)

  -- if/major ... having
  If cond thn mEls
    -> let cc       = compileExpr env regA cond
           (_, nTh, thC) = foldl genStmt ( []:env, nxt, [] ) thn
           (_, nEl, elC) = case mEls of
             Just els -> foldl genStmt ( []:env, nxt, [] ) els
             Nothing  -> ( []:env, nxt, [] )
           nk       = max nTh nEl
           tL       = length thC
           eL       = length elC
           bc       = cc
                    ++ [ Branch regA (Rel (tL + 2)) ]
                    ++ thC
                    ++ [ Jump (Rel (eL + 1)) ]
                    ++ elC
       in (env, nk, code ++ bc)

  -- while/fun
  While cond body
    -> let cc       = compileExpr env regA cond
           (_, nBd, bdC) = foldl genStmt ( []:env, nxt, [] ) body
           cL       = length cc
           bL       = length bdC
           lc       = cc
                    ++ [ Branch regA (Rel 2), Jump (Rel (bL + 2)) ]
                    ++ bdC
                    ++ [ Jump (Rel (-(cL + bL + 2))) ]
       in (env, nBd, code ++ lc)

  -- fork/join
  ForkJoin ss
    -> let (_:outer, nxt', fjC) = foldl genStmt ( []:env, nxt, [] ) ss
       in (outer, nxt', code ++ fjC)

  -- lock
  Lock _ ss
    -> let (_:outer, nxt', lkC) = foldl genStmt ( []:env, nxt, [] ) ss
       in (outer, nxt', code ++ lkC)

-- | Expression codegen
type Reg = Int
compileExpr :: Env -> Reg -> Expr -> [Instruction]
compileExpr env tgt expr = case expr of
  IntLit n      -> [ Load (ImmValue (fromIntegral n)) tgt ]
  BoolLit b     -> [ Load (ImmValue (if b then 1 else 0)) tgt ]
  StringLit s   -> error "String literal not allowed in expr context"
  Var x         -> [ Load (DirAddr (lookupScalar x env)) tgt ]
  ArrayRef nm i ->
    let (b, _, _) = lookupVector nm env
        iC     = compileExpr env regB i
        bC     = [ Load (ImmValue b) regC ]
        aC     = [ Compute Add regC regB regC ]
        lC     = [ Load (IndAddr regC) tgt ]
    in iC ++ bC ++ aC ++ lC

  BinOp op l r  -> let lC = compileExpr env tgt l
                       rC = compileExpr env regB r
                   in lC ++ rC ++ [ Compute (toOp op) tgt regB tgt ]
  UnOp "!" e  -> compileExpr env tgt e ++ [ Compute Xor tgt tgt tgt ]
  _             -> error $ "Unsupported Expr: " ++ show expr

-- | Operator mapping
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
