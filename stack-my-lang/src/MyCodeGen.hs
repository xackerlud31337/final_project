module MyCodeGen
  ( compileSource
  , compileAST
  ) where

import Prelude hiding (lookup)
import Data.List (lookup)
import Data.Char (ord)
import MyParser (parseMyLang, Stmt(..), Expr(..))
import Sprockell


data ElementType = ElementInt | ElementChar deriving (Eq, Show)
data VariableInfo
  = ScalarVariable Int
  | VectorVariable Int Int ElementType

type Scope = [(String, VariableInfo)]
type Environment = [Scope]
type MemorySlot = Int
type Register = Int


compileSource :: String -> [Instruction]
compileSource source =
  case parseMyLang source of
    Left parseError  -> error $ show parseError
    Right ast        -> compileAST ast


compileAST :: [Stmt] -> [Instruction]
compileAST statements =
  let initialEnvironment = [ [] ]
      initialMemorySlot = 0
      (_, _, instructions) = foldl generateStatement (initialEnvironment, initialMemorySlot, []) statements
  in instructions ++ [EndProg]


lookupScalar :: String -> Environment -> Int
lookupScalar variableName [] = error $ "Not found or not scalar: " ++ variableName
lookupScalar variableName (scope:outerScopes) =
  case lookup variableName scope of
    Just (ScalarVariable slot) -> slot
    _                         -> lookupScalar variableName outerScopes


lookupVector :: String -> Environment -> (Int, Int, ElementType)
lookupVector variableName [] = error $ "Not found or not vector: " ++ variableName
lookupVector variableName (scope:outerScopes) =
  case lookup variableName scope of
    Just (VectorVariable base length elementType) -> (base, length, elementType)
    _                                            -> lookupVector variableName outerScopes


generateStatement :: (Environment, MemorySlot, [Instruction]) -> Stmt -> (Environment, MemorySlot, [Instruction])
generateStatement (environment, nextSlot, instructions) statement = case statement of
  Decl _ variableName
    | variableName `elem` map fst (head environment)
      -> error $ "Variable already declared: " ++ variableName
    | otherwise
      -> let slot = nextSlot
             initInstructions = [ Load (ImmValue 0) regA, Store regA (DirAddr slot) ]
             newScope = (variableName, ScalarVariable slot) : head environment
         in (newScope : tail environment, slot + 1, instructions ++ initInstructions)

  DeclStr variableName stringValue
    | variableName `elem` map fst (head environment)
      -> error $ "Variable already declared: " ++ variableName
    | otherwise
      -> let strLength = length stringValue
             baseSlot = nextSlot
             charInstructions = concat [ [ Load (ImmValue (ord char)) regA
                                         , Store regA (DirAddr (baseSlot + i)) ]
                                       | (i, char) <- zip [0..] stringValue ]
             terminatorInstructions = [ Load (ImmValue 0) regA, Store regA (DirAddr (baseSlot + strLength)) ]
             newScope = (variableName, VectorVariable baseSlot (strLength + 1) ElementChar) : head environment
         in (newScope : tail environment, baseSlot + strLength + 1, instructions ++ charInstructions ++ terminatorInstructions)

  DeclVec variableName elements
    | variableName `elem` map fst (head environment)
      -> error $ "Variable already declared: " ++ variableName
    | otherwise
      -> let vecLength = length elements
             baseSlot = nextSlot
             storeInstructions = concat [ compileExpr environment regA expr ++ [ Store regA (DirAddr (baseSlot + i)) ]
                                       | (i, expr) <- zip [0..] elements ]
             newScope = (variableName, VectorVariable baseSlot vecLength ElementInt) : head environment
         in (newScope : tail environment, baseSlot + vecLength, instructions ++ storeInstructions)

  Assign variableName expr
    -> let slot = lookupScalar variableName environment
           exprInstructions = compileExpr environment regA expr
       in (environment, nextSlot, instructions ++ exprInstructions ++ [ Store regA (DirAddr slot) ])

  Print expr ->
    let exprInstructions = compileExpr environment regA expr
        device = case expr of
          StringLit _  -> charIO
          ArrayRef _ _ -> charIO
          _            -> numberIO
    in (environment, nextSlot, instructions ++ exprInstructions ++ [ WriteInstr regA device ])

  Block statements ->
    let (_:outerScopes, nextSlot', blockInstructions) = foldl generateStatement ([]:environment, nextSlot, []) statements
    in (outerScopes, nextSlot', instructions ++ blockInstructions)

  If condition thenBranch maybeElseBranch ->
    let condInstructions = compileExpr environment regA condition
        (_, thenNextSlot, thenInstructions) = foldl generateStatement ([]:environment, nextSlot, []) thenBranch
        (_, elseNextSlot, elseInstructions) = case maybeElseBranch of
                                                Just elseBranch -> foldl generateStatement ([]:environment, nextSlot, []) elseBranch
                                                Nothing        -> ([]:environment, nextSlot, [])
        thenLength = length thenInstructions
        elseLength = length elseInstructions
        zeroInstructions = [ Load (ImmValue 0) regB
                          , Compute Equal regA regB regA ]
        branchInstructions = zeroInstructions
                          ++ [ Branch regA (Rel (thenLength + 2)) ]
                          ++ thenInstructions
                          ++ [ Jump (Rel (elseLength + 1)) ]
                          ++ elseInstructions
    in (environment, max thenNextSlot elseNextSlot, instructions ++ condInstructions ++ branchInstructions)

  While condition bodyStatements
    -> let condInstructions = compileExpr environment regA condition
           (_, bodyNextSlot, bodyInstructions) = foldl generateStatement ([]:environment, nextSlot, []) bodyStatements
           condLength = length condInstructions
           bodyLength = length bodyInstructions
           loopInstructions = condInstructions ++ [ Branch regA (Rel 2), Jump (Rel (bodyLength + 2)) ] ++ bodyInstructions ++ [ Jump (Rel (-(condLength + bodyLength + 2))) ]
       in (environment, bodyNextSlot, instructions ++ loopInstructions)

  ForkJoin statements ->
    let (_:outerScopes, nextSlot', forkJoinInstructions) = foldl generateStatement ([]:environment, nextSlot, []) statements
    in (outerScopes, nextSlot', instructions ++ forkJoinInstructions)

  Lock _ statements ->
    let (_:outerScopes, nextSlot', lockInstructions) = foldl generateStatement ([]:environment, nextSlot, []) statements
    in (outerScopes, nextSlot', instructions ++ lockInstructions)


compileExpr :: Environment -> Register -> Expr -> [Instruction]
compileExpr = compileOr


compileOr :: Environment -> Register -> Expr -> [Instruction]
compileOr environment targetRegister expr =
  let (first, rest) = splitOp "||" expr
      firstCode = compileAnd environment targetRegister first
      restCode = concat [ compileAnd environment regB e ++ [ Compute Or targetRegister regB targetRegister ] | e <- rest ]
  in firstCode ++ restCode


compileAnd :: Environment -> Register -> Expr -> [Instruction]
compileAnd environment targetRegister expr =
  let (first, rest) = splitOp "&&" expr
      firstCode = compileEq environment targetRegister first
      restCode = concat [ compileEq environment regB e ++ [ Compute And targetRegister regB targetRegister ] | e <- rest ]
  in firstCode ++ restCode


compileEq :: Environment -> Register -> Expr -> [Instruction]
compileEq environment targetRegister expr =
  let (first, rest)  = splitOpList [("==", Equal), ("!=", NEq)] expr
      firstCode = compileCmp environment targetRegister first
      restCode = concat [ compileCmp environment regB e ++ [ Compute op targetRegister regB targetRegister ]
                       | (_, op, e) <- rest ]
  in firstCode ++ restCode


compileCmp :: Environment -> Register -> Expr -> [Instruction]
compileCmp environment targetRegister expr =
  let (first, rest) = splitOpList [("<", Lt), ("<=", LtE), (">", Gt), (">=", GtE)] expr
      firstCode = compileAdd environment targetRegister first
      restCode = concat [ compileAdd environment regB e ++ [ Compute op targetRegister regB targetRegister ]
                       | (_, op, e) <- rest ]
  in firstCode ++ restCode


compileAdd :: Environment -> Register -> Expr -> [Instruction]
compileAdd environment targetRegister expr =
  let (first, rest) = splitOpList [("+", Add), ("-", Sub)] expr
      firstCode = compileTerm environment targetRegister first
      restCode = concat [ compileTerm environment regB e ++ [ Compute op targetRegister regB targetRegister ]
                       | (_, op, e) <- rest ]
  in firstCode ++ restCode


compileTerm :: Environment -> Register -> Expr -> [Instruction]
compileTerm environment targetRegister expr =
  let (first, rest) = splitOp "*" expr
      firstCode = compileFactor environment targetRegister first
      restCode = concat [ compileFactor environment regC e ++ [ Compute Mul targetRegister regC targetRegister ]
                       | e <- rest ]
  in firstCode ++ restCode


compileFactor :: Environment -> Register -> Expr -> [Instruction]
compileFactor environment targetRegister expr = case expr of
  IntLit n      -> [ Load (ImmValue (fromIntegral n)) targetRegister ]
  BoolLit b     -> [ Load (ImmValue (if b then 1 else 0)) targetRegister ]
  Var variableName -> [ Load (DirAddr (lookupScalar variableName environment)) targetRegister ]
  ArrayRef arrayName indexExpr ->
    let (base, _, _) = lookupVector arrayName environment
        indexCode = compileExpr environment regB indexExpr
        baseCode = [ Load (ImmValue base) regC ]
        addressCode = [ Compute Add regC regB regC ]
        loadCode = [ Load (IndAddr regC) targetRegister ]
    in indexCode ++ baseCode ++ addressCode ++ loadCode
  UnOp "!" e  -> compileExpr environment targetRegister e ++ [ Compute Xor targetRegister targetRegister targetRegister ]
  _           -> error $ "Unsupported factor: " ++ show expr


defaultSplit :: String -> Expr -> (Expr, [Expr])
defaultSplit op (BinOp symbol left right) | symbol == op = let (left0, rest) = defaultSplit op left in (left0, rest ++ [right])
defaultSplit _ expr = (expr, [])

splitOp :: String -> Expr -> (Expr, [Expr])
splitOp = defaultSplit

splitOpList :: [(String, Operator)] -> Expr -> (Expr, [(String, Operator, Expr)])
splitOpList operators expr = case expr of
  BinOp symbol left right -> case lookup symbol operators of
    Just op -> let (left0, rest) = splitOpList operators left in (left0, rest ++ [(symbol, op, right)])
    Nothing -> (expr, [])
  _ -> (expr, [])

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
toOp _     = error "Unsupported in toOp"
