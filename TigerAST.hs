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

data Ty = NameTy Symbol SourcePos
        | RecordTy [Field]
        | ArrayTy Symbol SourcePos

data Oper = PlusOp | MinusOp | TimesOp | DivideOp
          | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

type Field = (Symbol,Symbol,SourcePos)
type FunDec = (Symbol, [Field], Maybe (Symbol, SourcePos), Exp, SourcePos)


