{-# LANGUAGE LambdaCase #-}
module MyCodeGen
  ( compileSource
  , compileAST
  ) where

import Prelude hiding (lookup)
import Data.List (lookup)
import Data.Char (ord)
import MyParser (parseMyLang, Stmt(..), Expr(..))
import Sprockell

--Types & environment

data ElementType  = ElementInt | ElementChar deriving (Eq, Show)
data VariableInfo = ScalarVariable Int Bool  -- Int: slot, Bool: isShared
                  | VectorVariable Int Int ElementType
                  deriving (Show)

type Scope       = [(String, VariableInfo)]
type Environment = [Scope]
type MemorySlot  = Int
type Register    = Int

-- Debug helper
debugEnv :: String -> Environment -> String
debugEnv label env = label ++ ": " ++ show (map (map fst) env)

-- Compile string into multiple Sprockell programs
compileSource :: String -> [[Instruction]]
compileSource src =
  case parseMyLang src of
    Left err  -> error $ show err
    Right ast -> 
      let concurrent = hasConcurrency ast
      in if concurrent
         then compileASTConcur ast
         else [compileSingleThread ast]
  where
    hasConcurrency [] = False
    hasConcurrency (ForkJoin _ : _) = True
    hasConcurrency (Block blkStmts : rest) = hasConcurrency blkStmts || hasConcurrency rest
    hasConcurrency (_:rest) = hasConcurrency rest

-- single thread
compileAST :: [Stmt] -> [Instruction]
compileAST stmts = 
  if hasConcurrency stmts
  then case compileASTConcur stmts of
         (m:_) -> m
         []    -> [EndProg]
  else compileSingleThread stmts
  where
    hasConcurrency [] = False
    hasConcurrency (ForkJoin _ : _) = True
    hasConcurrency (Block blkStmts : rest) = hasConcurrency blkStmts || hasConcurrency rest
    hasConcurrency (_:rest) = hasConcurrency rest

-- single thread w (no concurrency setup)
compileSingleThread :: [Stmt] -> [Instruction]
compileSingleThread stmts =
  let initialEnv = [[]]  -- empty , no special variables
      (_, _, instrs) = foldl (\(e,s,c) st -> generateStatement (e,s,c) False st)  -- not main
                              (initialEnv, 0, []) 
                              stmts
  in instrs ++ [EndProg]

-- join counter in slot 0 and mutex in slot 1
compileASTConcur :: [Stmt] -> [[Instruction]]
compileASTConcur stmts =
  let
    -- Shared memory slots
    joinSlot  = 0
    mutexSlot = 1
    nextFree  = 2

    initSetup =
      [ Load  (ImmValue 0) regA
      , WriteInstr regA (DirAddr joinSlot)    -- counter
      , WriteInstr regA (DirAddr mutexSlot)   -- mutex (unlocked)
      ]

    -- Building with special variables
    initialEnv = [[ ("joinCounter", ScalarVariable joinSlot True)
                  , ("joinMutex",   ScalarVariable mutexSlot True)
                  ]]
    
    -- Process all statements, collecting thread bodies
    (envAfter, slotAfter, mainBody) = processStatements initialEnv nextFree [] stmts

    -- Extract all thread bodies
    (mainCode, allThreadBodies) = extractThreads mainBody

    mainProg = initSetup ++ mainCode ++ [EndProg]

    -- initialization for children
    childProgs = [ body ++
        [ -- Atomic counter
          Load      (ImmValue 1)         regB
        , TestAndSet (DirAddr mutexSlot) -- Lock mutex
        , Receive regA                   -- Get old mutex value
        , Compute   Equal           regA regB regA
        , Branch    regA           (Rel 2)
        , Jump      (Rel (-5))
          -- Critical section: increment counter
        , ReadInstr (DirAddr joinSlot)
        , Receive   regB
        , Load      (ImmValue 1)         regC
        , Compute   Add            regB regC regB
        , WriteInstr regB           (DirAddr joinSlot)
          -- Unlock mutex
        , Load      (ImmValue 0)         regA
        , WriteInstr regA           (DirAddr mutexSlot)
        , EndProg
        ]
      | body <- allThreadBodies
      ]
  in mainProg : childProgs

-- handling fork-join 
processStatements :: Environment -> MemorySlot -> [Either [Instruction] [[Instruction]]] -> [Stmt] -> (Environment, MemorySlot, [Either [Instruction] [[Instruction]]])
processStatements env slot acc [] = (env, slot, reverse acc)
processStatements env slot acc (stmt:rest) = case stmt of
  ForkJoin stmts ->
    let bodies = groupForkStatements env 0 stmts
        numThreads = length bodies
        waitCode = if numThreads > 0
                   then [ -- Wait for all children to complete
                          Load    (ImmValue numThreads) regB  -- Expected count
                        , ReadInstr (DirAddr 0)              -- Read join counter (slot 0)
                        , Receive regA                       -- Current count
                        , Compute Equal regA regB regA      -- Check if all done
                        , Branch  regA (Rel 2)              -- If done, skip jump
                        , Jump    (Rel (-4))                -- Loop back
                        ]
                   else []
    in processStatements env slot (Right bodies : Left waitCode : acc) rest
  _ ->
    let (env', slot', is) = generateStatement (env, slot, []) True stmt
    in processStatements env' slot' (Left is : acc) rest

extractThreads :: [Either [Instruction] [[Instruction]]] -> ([Instruction], [[Instruction]])
extractThreads [] = ([], [])
extractThreads (Left instrs : rest) = 
  let (mainCode, threads) = extractThreads rest
  in (instrs ++ mainCode, threads)
extractThreads (Right bodies : rest) =
  let (mainCode, threads) = extractThreads rest
  in (mainCode, bodies ++ threads)

generateTopLevel
  :: (Environment, MemorySlot, [Instruction], [[Instruction]])
  -> Stmt
  -> (Environment, MemorySlot, [Instruction], [[Instruction]])
generateTopLevel (env, slot, mainI, threads) stmt = case stmt of
  ForkJoin stmts ->
    let bodies = groupForkStatements env 0 stmts
        numThreads = length bodies
        waitCode = if numThreads > 0
                   then [ -- Wait for all children to complete
                          Load    (ImmValue numThreads) regB  -- Expected count
                        , ReadInstr (DirAddr 0)              -- Read join counter (slot 0)
                        , Receive regA                       -- Current count
                        , Compute Equal regA regB regA      -- Check if all done
                        , Branch  regA (Rel 2)              -- If done, skip jump
                        , Jump    (Rel (-4))                -- Loop back
                        ]
                   else []
    in (env, slot, mainI ++ waitCode, threads ++ bodies)
  _ ->
    let (env', slot', is) = generateStatement (env, slot, []) True stmt
    in (env', slot', mainI ++ is, threads)

-- child threads - all parent vars become shared
makeChildEnvironment :: Environment -> Environment
makeChildEnvironment env = map makeShared env
  where
    makeShared scope = [(name, makeVarShared var) | (name, var) <- scope]
    makeVarShared (ScalarVariable slot _) = ScalarVariable slot True
    makeVarShared v = v

-- fork statements into separate threads
groupForkStatements :: Environment -> MemorySlot -> [Stmt] -> [[Instruction]]
groupForkStatements parentEnv _ stmts =  
  let threadGroups = extractThreadGroups stmts
      childEnv = makeChildEnvironment parentEnv
      threadEnv = [] : childEnv
      compileThread statements = 
        let (_, _, code) = foldl 
              (\(e, s, c) stmt -> generateStatement (e, s, c) False stmt)
              (threadEnv, 0, [])
              statements
        in code
  in map compileThread threadGroups

extractThreadGroups :: [Stmt] -> [[Stmt]]
extractThreadGroups stmts = 
  let groups = groupStatements stmts []
  in filter (not . null) groups
  where
    -- Group statements that belong together
    groupStatements [] acc = reverse acc
    groupStatements (s:ss) acc = case s of
      -- Pattern: Decl -> assignment & while loop
      Decl t v -> case ss of
        (Assign v' init : rest) | v == v' -> 
          case rest of
            (w@(While _ _) : rest') -> 
              groupStatements rest' ([Decl t v, Assign v' init, w] : acc)
            _ -> groupStatements rest ([Decl t v, Assign v' init] : acc)
        _ -> groupStatements ss ([s] : acc)
      -- Pattern: Assignment -> while loop
      Assign v init -> case ss of
        (w@(While _ _) : rest) -> 
          groupStatements rest ([Assign v init, w] : acc)
        _ -> groupStatements ss ([s] : acc)
      -- only those loops or statements
      _ -> groupStatements ss ([s] : acc)

generateStatement
  :: (Environment, MemorySlot, [Instruction])
  -> Bool
  -> Stmt
  -> (Environment, MemorySlot, [Instruction])
generateStatement ([], _, _) _ stmt = error "Empty environment"
generateStatement (env@(current:rest), nextSlot, instrs) isMainThread stmt = 
  let hasConcurrencyInProgram = any (\scope -> "joinCounter" `elem` map fst scope) env
  in case stmt of
  Decl _ name
    | name `elem` map fst current
      -> error $ "Variable already declared: " ++ name
    | otherwise
      -> let slot = nextSlot
             -- Only use shared memory if we're in a concurrent program's main thread
             isShared = isMainThread && hasConcurrencyInProgram
             code = [ Load (ImmValue 0) regA ] ++
                    if isShared 
                    then [ WriteInstr regA (DirAddr slot) ]
                    else [ Store regA (DirAddr slot) ]
             newScope = (name, ScalarVariable slot isShared) : current
         in (newScope:rest, slot+1, instrs ++ code)

  Assign name expr
    -> let varInfo = lookupVar name env
           code = case varInfo of
             Just (slot, isShared) ->
               compileExpr env regA expr ++ 
               if isShared
               then [ WriteInstr regA (DirAddr slot) ]
               else [ Store regA (DirAddr slot) ]
             Nothing -> error $ "Variable not found: " ++ name ++ " in env: " ++ debugEnv "Assign" env
       in (env, nextSlot, instrs ++ code)

  Print expr
    -> let code = compileExpr env regA expr
           dev = case expr of 
             StringLit _ -> charIO
             ArrayRef{} -> charIO
             _ -> numberIO
       in (env, nextSlot, instrs ++ code ++ [ WriteInstr regA dev ])

  Block stmts
    -> let
           newEnv = [] : env
           (env', slot', codes) = foldl (\(e,s,c) st -> generateStatement (e,s,c) isMainThread st) 
                                        (newEnv, nextSlot, []) 
                                        stmts
           envAfterBlock = case env' of
             (_:rest) -> rest
             [] -> error "Environment corruption in block"
       in (envAfterBlock, slot', instrs ++ codes)

  If cond thenB mayElse
    -> let condCode = compileExpr env regA cond
           ( _, slotT, thenCode) = foldl (\(e,s,c) st -> generateStatement (e,s,c) isMainThread st) (env, nextSlot, []) thenB
           ( _, slotE, elseCode) = case mayElse of
                                     Just es -> foldl (\(e,s,c) st -> generateStatement (e,s,c) isMainThread st) (env, nextSlot, []) es
                                     Nothing -> (env, nextSlot, [])
           lenT = length thenCode; lenE = length elseCode
           branch = [ Load (ImmValue 0) regB
                    , Compute Equal regA regB regA
                    , Branch regA (Rel (lenT + 2)) ]
                    ++ thenCode ++ [ Jump (Rel (lenE + 1)) ] ++ elseCode
           nextSlot' = max slotT slotE
       in (env, nextSlot', instrs ++ condCode ++ branch)

  While cond body
    -> let condCode = compileExpr env regA cond
           (_, slotB, bodyCode) = foldl (\(e,s,c) st -> generateStatement (e,s,c) isMainThread st) (env, nextSlot, []) body
           lC = length condCode; lB = length bodyCode
           loop = condCode ++ [ Branch regA (Rel 2), Jump (Rel (lB + 2)) ]
                  ++ bodyCode ++ [ Jump (Rel (-(lC + lB + 2))) ]
       in (env, slotB, instrs ++ loop)

  ForkJoin _ -> error "ForkJoin only at top-level"

  Lock name stmts
    -> let varInfo = lookupVar name env
           (lockCode, unlockCode) = case varInfo of
             Just (slot, _) ->  -- Lock variables are always in shared memory
               let -- Atomic lock
                   lock = [ Load      (ImmValue 1)         regB
                          , TestAndSet (DirAddr slot)
                          , Receive   regA
                          , Compute   Equal           regA regB regA
                          , Branch    regA           (Rel 2)
                          , Jump      (Rel (-5))
                          ]
                   unlock = [ Load (ImmValue 0) regA
                            , WriteInstr regA (DirAddr slot) ]
               in (lock, unlock)
             Nothing -> error $ "Lock variable not found: " ++ name ++ " in env: " ++ debugEnv "Lock" env
           (_, slot', bodyCode) = foldl (\(e,s,c) st -> generateStatement (e,s,c) isMainThread st) 
                                        (env, nextSlot, []) 
                                        stmts
       in (env, slot', instrs ++ lockCode ++ bodyCode ++ unlockCode)

  DeclStr name s
    | name `elem` map fst current
      -> error $ "Variable already declared: " ++ name
    | otherwise
      -> let base = nextSlot
             isShared = isMainThread && hasConcurrencyInProgram
             storeChars = concat [ [ Load (ImmValue (ord c)) regA ] ++
                                   if isShared
                                   then [ WriteInstr regA (DirAddr (base+i)) ]
                                   else [ Store regA (DirAddr (base+i)) ]
                                 | (i,c) <- zip [0..] s ]
             terminator = [ Load (ImmValue 0) regA ] ++
                         if isShared
                         then [ WriteInstr regA (DirAddr (base + length s)) ]
                         else [ Store regA (DirAddr (base + length s)) ]
             newScope = (name, VectorVariable base (length s + 1) ElementChar) : current
         in (newScope:rest, base + length s + 1, instrs ++ storeChars ++ terminator)

  DeclVec name es
    | name `elem` map fst current
      -> error $ "Variable already declared: " ++ name
    | otherwise
      -> let base = nextSlot
             isShared = isMainThread && hasConcurrencyInProgram
             stores = concat [ compileExpr env regA e ++ 
                              if isShared
                              then [ WriteInstr regA (DirAddr (base+i)) ]
                              else [ Store regA (DirAddr (base+i)) ]
                             | (i,e) <- zip [0..] es ]
             newScope = (name, VectorVariable base (length es) ElementInt) : current
         in (newScope:rest, base + length es, instrs ++ stores)

compileExpr :: Environment -> Register -> Expr -> [Instruction]
compileExpr = compileOr

compileOr :: Environment -> Register -> Expr -> [Instruction]
compileOr env tgt = \case
  BinOp "||" l r -> 
    let lCode = compileAnd env tgt l
        rCode = compileAnd env regB r
    in lCode ++ rCode ++ [ Compute Or tgt regB tgt ]
  e -> compileAnd env tgt e

compileAnd :: Environment -> Register -> Expr -> [Instruction]
compileAnd env tgt = \case
  BinOp "&&" l r -> 
    let lCode = compileCmp env tgt l
        rCode = compileCmp env regB r
    in lCode ++ rCode ++ [ Compute And tgt regB tgt ]
  e -> compileCmp env tgt e

compileCmp :: Environment -> Register -> Expr -> [Instruction]
compileCmp env tgt = \case
  BinOp "==" l r -> 
    let lCode = compileAdd env tgt l
        rCode = compileAdd env regC r
    in lCode ++ rCode ++ [ Compute Equal tgt regC tgt ]
  BinOp "!=" l r -> 
    let lCode = compileAdd env tgt l
        rCode = compileAdd env regC r
    in lCode ++ rCode ++ [ Compute NEq tgt regC tgt ]
  BinOp "<"  l r -> 
    let lCode = compileAdd env tgt l
        rCode = compileAdd env regC r
    in lCode ++ rCode ++ [ Compute Lt tgt regC tgt ]
  BinOp ">=" l r -> 
    let lCode = compileAdd env tgt l
        rCode = compileAdd env regC r
    in lCode ++ rCode ++ [ Compute GtE tgt regC tgt ]
  BinOp "<=" l r -> 
    let lCode = compileAdd env tgt l
        rCode = compileAdd env regC r
    in lCode ++ rCode ++ [ Compute LtE tgt regC tgt ]
  BinOp ">"  l r -> 
    let lCode = compileAdd env tgt l
        rCode = compileAdd env regC r
    in lCode ++ rCode ++ [ Compute Gt tgt regC tgt ]
  e -> compileAdd env tgt e

compileAdd :: Environment -> Register -> Expr -> [Instruction]
compileAdd env tgt = \case
  BinOp "+" l r -> 
    -- For complex right operands, use stack to preserve left value
    let lCode = compileTerm env tgt l  -- Compile left to target
        -- Push left value to stack to preserve it
        pushLeft = [ Push tgt ]
        -- Compile right to target
        rCode = compileTerm env tgt r
        -- Pop left value and perform addition
        popAndAdd = [ Pop regB  -- Pop left value to regB
                    , Compute Add regB tgt tgt ]
    in lCode ++ pushLeft ++ rCode ++ popAndAdd
  BinOp "-" l r -> 
    -- Similar approach for subtraction
    let lCode = compileTerm env tgt l
        pushLeft = [ Push tgt ]
        rCode = compileTerm env tgt r
        popAndSub = [ Pop regB
                    , Compute Sub regB tgt tgt ]
    in lCode ++ pushLeft ++ rCode ++ popAndSub
  e -> compileTerm env tgt e

compileTerm :: Environment -> Register -> Expr -> [Instruction]
compileTerm env tgt = \case
  BinOp "*" l r -> 
    -- For multiplication, also use stack
    let lCode = compileFactor env tgt l
        pushLeft = [ Push tgt ]
        rCode = compileFactor env tgt r
        popAndMul = [ Pop regB
                    , Compute Mul regB tgt tgt ]
    in lCode ++ pushLeft ++ rCode ++ popAndMul
  e -> compileFactor env tgt e

compileFactor :: Environment -> Register -> Expr -> [Instruction]
compileFactor env tgt = \case
  IntLit n      -> [ Load (ImmValue (fromIntegral n)) tgt ]
  BoolLit b     -> [ Load (ImmValue (if b then 1 else 0)) tgt ]
  Var v         -> 
    -- Special handling for boolean keywords
    case v of
      "true"  -> [ Load (ImmValue 1) tgt ]
      "false" -> [ Load (ImmValue 0) tgt ]
      "a"     -> [ Load (ImmValue 1) tgt ]  -- true
      "cs"    -> [ Load (ImmValue 0) tgt ]  -- false
      _ -> case lookupVar v env of
             Just (slot, isShared) ->
               if isShared
               then [ ReadInstr (DirAddr slot)
                    , Receive tgt ]
               else [ Load (DirAddr slot) tgt ]
             Nothing -> error $ "Variable not found: " ++ v ++ " in env: " ++ debugEnv "Var" env
  ArrayRef a ix ->
    let
        (base, len, typ, isShared) = lookupVectorFull a env
        idxCode = compileExpr env regA ix
        addrCalc = [ Load (ImmValue base) regB
                   , Compute Add regB regA regA ]
        loadCode = if isShared
                   then [ ReadInstr (IndAddr regA)
                        , Receive tgt ]
                   else [ Load (IndAddr regA) tgt ]
    in idxCode ++ addrCalc ++ loadCode
  UnOp "!" e    -> 
    let eCode = compileExpr env tgt e
    in eCode ++ [ Compute Equal tgt reg0 tgt ]
  StringLit _ -> error "String literals cannot be used as values"
  UnOp op _ -> error $ "Unsupported unary operator: " ++ op
  e@(BinOp _ _ _) -> compileExpr env tgt e

-- Helper to lookup variable through all scopes
lookupVar :: String -> Environment -> Maybe (Int, Bool)
lookupVar n [] = Nothing
lookupVar n (scope:rest) = 
  case lookup n scope of
    Just (ScalarVariable i shared) -> Just (i, shared)
    Just _ -> Nothing
    Nothing -> lookupVar n rest  -- Look in parent scopes

-- Lookup helpers - search through all scopes
lookupScalar :: String -> Environment -> (Int, Bool)
lookupScalar n env = 
  case lookupVar n env of
    Just info -> info
    Nothing -> error $ "Variable not found: " ++ n

lookupVector :: String -> Environment -> (Int, Int, ElementType)
lookupVector n [] = error $ "Variable not found: " ++ n
lookupVector n (scope:rest) = 
  case lookup n scope of
    Just (VectorVariable b l t) -> (b, l, t)
    Just _ -> error $ "Not a vector: " ++ n
    Nothing -> lookupVector n rest

lookupVectorFull :: String -> Environment -> (Int, Int, ElementType, Bool)
lookupVectorFull n env = 
  let hasConcurrency = any (\scope -> "joinCounter" `elem` map fst scope) env
      helper [] = error $ "Variable not found: " ++ n
      helper (scope:rest) = 
        case lookup n scope of
          Just (VectorVariable b l t) -> 
            (b, l, t, hasConcurrency)
          Just _ -> error $ "Not a vector: " ++ n
          Nothing -> helper rest
  in helper env