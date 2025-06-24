module MyParser
    ( parseMyLang
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Expr as Ex
import Control.Arrow (left)
import Data.Functor.Identity(Identity)


--AST definitions
data Type = TypeInt | TypeBool deriving (Show, Eq)

data Expr
    = IntLit Integer
    | BoolLit Bool
    | Var String
    | BinOp String Expr Expr
    | UnOp String Expr
    deriving (Show, Eq)

data Stmt
    = Decl Type String 
    | Assign String Expr
    | Print Expr
    | If Expr [Stmt] (Maybe [Stmt])
    | While Expr [Stmt]
    | ForkJoin [Stmt]
    | Lock String [Stmt]
    | Block [Stmt]
    deriving (Show, Eq)

-- Lexer
languageDef = emptyDef
  { Tok.commentLine     = "//"
  , Tok.reservedNames   = ["int", "bool", "true", "false", "if", "else", "while", "print", "fork", "join", "lock"]
  , Tok.reservedOpNames = ["+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "&&", "||", "!", "="]
  }

lexer = Tok.makeTokenParser languageDef

identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
parens = Tok.parens lexer
braces = Tok.braces lexer
semi = Tok.semi lexer
integer = Tok.integer lexer
whiteSpace = Tok.whiteSpace lexer

--Expression parser 
expr :: Parser Expr
expr = Ex.buildExpressionParser table term

term :: Parser Expr
term = parens expr
    <|> (BoolLit True <$ reserved "true")
    <|> (BoolLit False <$ reserved "false")
    <|> (IntLit <$> integer)
    <|> (Var <$> identifier)

table :: [[Ex.Operator String () Identity Expr]]
table =
  [ [prefix "!" UnOp]
  , [binary "*" (BinOp "*") Ex.AssocLeft, binary "/" (BinOp "/") Ex.AssocLeft]
  , [binary "+" (BinOp "+") Ex.AssocLeft, binary "-" (BinOp "-") Ex.AssocLeft]
  , [binary "==" (BinOp "==") Ex.AssocNone, binary "!=" (BinOp "!=") Ex.AssocNone
    , binary "<" (BinOp "<") Ex.AssocNone, binary "<=" (BinOp "<=") Ex.AssocNone
    , binary ">" (BinOp ">") Ex.AssocNone, binary ">=" (BinOp ">=") Ex.AssocNone]
  , [binary "&&" (BinOp "&&") Ex.AssocRight]
  , [binary "||" (BinOp "||") Ex.AssocRight]
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary name fun = Ex.Infix (reservedOp name >> return fun)

prefix :: String -> (String -> Expr -> Expr) -> Ex.Operator String () Identity Expr
prefix name fun = Ex.Prefix (reservedOp name >> return (fun name))

--Statement parsers
stmt :: Parser Stmt
stmt = try decl 
    <|> try assign 
    <|> try printStmt
    --additional statement types will go here later
    <|> try ifStmt
    <|> try whileStmt
    <|> try forkJoinStmt
    <|> try lockStmt
    <|> block

forkJoinStmt :: Parser Stmt
forkJoinStmt = do
    reserved "fork"
    Block body <- block
    reserved "join"
    semi
    return $ ForkJoin body

lockStmt :: Parser Stmt
lockStmt = do
    reserved "lock"
    lockName <- identifier
    Block body <- block
    return $ Lock lockName body

decl :: Parser Stmt
decl = do
    typ <- (TypeInt <$ reserved "int") <|> (TypeBool <$ reserved "bool") 
    varName <- identifier 
    semi 
    return $ Decl typ varName

assign :: Parser Stmt
assign = do
    varName <- identifier
    reservedOp "="
    e <- expr
    semi 
    return $ Assign varName e

printStmt :: Parser Stmt
printStmt = do
    reserved "print"
    e <- expr
    semi
    return $ Print e

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- parens expr
    Block thenBlock <- block
    elseBlock <- optionMaybe (reserved "else" >> (do Block b <- block; return b))
    return $ If cond thenBlock elseBlock

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- parens expr
    Block body <- block
    return $ While cond body

block :: Parser Stmt
block = Block <$> braces (many stmt)

--Placeholder top-lecel program parser
program :: Parser [Stmt]
program = whiteSpace >> many stmt

--Public function
parseMyLang :: String -> Either String [Stmt]
parseMyLang = left show . parse (program <* eof) "<stdin>"