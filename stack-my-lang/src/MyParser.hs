module MyParser
    ( parseMyLang, Stmt(..), Expr(..)
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Expr as Ex
import Control.Arrow (left)
import Data.Functor.Identity (Identity)

-- | Types in the language
data Type = TypeInt | TypeBool
  deriving (Show, Eq)

-- | Expression AST
data Expr
  = IntLit Integer
  | BoolLit Bool
  | Var String
  | BinOp String Expr Expr
  | UnOp  String Expr
  deriving (Show, Eq)

-- | Statement AST
data Stmt
  = Decl   Type String
  | Assign String Expr
  | Print  Expr
  | If     Expr [Stmt] (Maybe [Stmt])
  | While  Expr [Stmt]
  | ForkJoin [Stmt]
  | Lock   String [Stmt]
  | Block  [Stmt]
  deriving (Show, Eq)

-- Lexer
-- the (i) will replace the (int) 
-- the (am) will replace the (bool)
-- the (a) will replace the (true)
-- the (cs) will replace the (false)
-- the (major) will replace the (if)
-- the (having) will replace the (else) 
-- the (fun) will replace the (while)
languageDef :: Tok.LanguageDef ()
languageDef = emptyDef
  { Tok.commentLine     = "//"
  , Tok.reservedNames   = ["i","am","a","cs","major","having","fun","print","fork","join","lock"]
  , Tok.reservedOpNames = ["+","-","*","==","!=","<","<=" ,">",">=","&&","||","!","="]
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser languageDef

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

semi :: Parser String
semi = Tok.semi lexer

integer :: Parser Integer
integer = Tok.integer lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- Expression parser
expr :: Parser Expr
expr = Ex.buildExpressionParser table term

term :: Parser Expr
term =  parens expr
    <|> (BoolLit True  <$ reserved "a")
    <|> (BoolLit False <$ reserved "cs")
    <|> (IntLit   <$> integer)
    <|> (Var      <$> identifier)

table :: [[Ex.Operator String () Identity Expr]]
table =
  [ [prefix "!" UnOp]
  , [binary "*" (BinOp "*") Ex.AssocLeft]
  , [binary "+" (BinOp "+") Ex.AssocLeft, binary "-" (BinOp "-") Ex.AssocLeft]
  , [ binary "==" (BinOp "==") Ex.AssocNone
    , binary "!=" (BinOp "!=") Ex.AssocNone
    , binary "<"  (BinOp "<")  Ex.AssocNone
    , binary "<=" (BinOp "<=") Ex.AssocNone
    , binary ">"  (BinOp ">")  Ex.AssocNone
    , binary ">=" (BinOp ">=") Ex.AssocNone
    ]
  , [binary "&&" (BinOp "&&") Ex.AssocRight]
  , [binary "||" (BinOp "||") Ex.AssocRight]
  ]

binary :: String
       -> (Expr -> Expr -> Expr)
       -> Ex.Assoc
       -> Ex.Operator String () Identity Expr
binary name fun assoc =
  Ex.Infix (reservedOp name >> return fun) assoc

prefix :: String
       -> (String -> Expr -> Expr)
       -> Ex.Operator String () Identity Expr
prefix name fun =
  Ex.Prefix (reservedOp name >> return (fun name))

-- Statement parsers
stmt :: Parser Stmt
stmt =  try decl
    <|> try assign
    <|> try printStmt
    <|> try ifStmt
    <|> try whileStmt
    <|> try forkJoinStmt
    <|> try lockStmt
    <|> block

decl :: Parser Stmt
decl = do
  t <- (TypeInt <$ reserved "i") <|> (TypeBool <$ reserved "am")
  v <- identifier
  _ <- semi
  return $ Decl t v

assign :: Parser Stmt
assign = do
  v <- identifier
  reservedOp "="
  e <- expr
  _ <- semi
  return $ Assign v e

printStmt :: Parser Stmt
printStmt = do
  reserved "print"
  e <- expr
  _ <- semi
  return $ Print e

ifStmt :: Parser Stmt
ifStmt = do
  reserved "major"
  c <- parens expr
  Block t <- block
  mb <- optionMaybe $ do
    reserved "having"
    Block b <- block
    return b
  return $ If c t mb

whileStmt :: Parser Stmt
whileStmt = do
  reserved "fun"
  c <- parens expr
  Block b <- block
  return $ While c b

forkJoinStmt :: Parser Stmt
forkJoinStmt = do
  reserved "fork"
  Block b <- block
  reserved "join"
  _ <- semi
  return $ ForkJoin b

lockStmt :: Parser Stmt
lockStmt = do
  reserved "lock"
  nm <- identifier
  Block b <- block
  return $ Lock nm b

block :: Parser Stmt
block = Block <$> braces (many stmt)

program :: Parser [Stmt]
program = whiteSpace >> many stmt <* eof

-- | The only function exposed
parseMyLang :: String -> Either String [Stmt]
parseMyLang = left show . parse program "<stdin>"