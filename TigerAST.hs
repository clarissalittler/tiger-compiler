module TigerAST where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type Symbol = String -- just for now

data Var = SimpleVar Symbol SourcePos
         | FieldVar Var Symbol SourcePos
         | SubscriptVar Var Exp SourcePos

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp String SourcePos
         | CallExp Symbol [Exp] SourcePos
         | OpExp Exp Oper Exp SourcePos
         | RecordExp [(Symbol, Exp, SourcePos)] Symbol SourcePos
         | SeqExp [(Exp, SourcePos)]
         | AssignExp Var Exp SourcePos
         | IfExp Exp Exp (Maybe Exp) SourcePos
         | WhileExp Exp Exp SourcePos
         | ForExp Symbol Exp Exp Exp SourcePos
         | BreakExp SourcePos
         | LetExp [Dec] Exp SourcePos
         | ArrayExp Symbol Exp Exp SourcePos

data Dec = FunctionDec [FunDec]
         | VarDec Symbol (Maybe (Symbol, SourcePos)) Exp SourcePos
         | TypeDec [(Symbol, Ty, SourcePos)]

data Ty = NameTy Symbol SourcePos -- names of types, int and string are pre-defined names
        | RecordTy Int -- UID 
                   [Field]
        | ArrayTy Symbol SourcePos

data Oper = PlusOp | MinusOp | TimesOp | DivideOp
          | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

type Field = (Symbol,Symbol,SourcePos)
type FunDec = (Symbol, [Field], Maybe (Symbol, SourcePos), Exp, SourcePos)

-- parsing mess

spacey :: Parser ()
spacey = L.space (spaceChar >> return ()) (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
lexeme = L.lexeme spacey
symbol = L.symbol spacey
braces = lexeme . between (string "{") (string "}")
brackets = lexeme . between (string "[") (string "]")
parens = lexeme . between (string "(") (string ")")
comma = symbol ","
commaSep = (flip sepBy1) comma

ident = do 
  s <- lowerChar 
  ds <- some (char '_' <|> digitChar <|> letterChar)
  return $ s : ds -- need to add a check that these aren't reserved words

parseDecl = parseFunDec <|> parseVarDec <|> parseTyDec

parseTypeA = undefined
  
parseVarDec = do
  symbol "var"
  id <- ident
  ty <- option Nothing parseTypeA
  symbol ":="
  e <- parseExp
  p <- getPosition
  return $ VerDec id ty e p

parseTyDec' = do
  symbol "type"
  n <- ident
  symbol "="
  t <- parseTy
  p <- getPosition
  return $ (n,t,p)
parseTyDec = TypeDec `fmap` (many parseTyDec')

