{
module Sodium.Pascal.Parse (parse, Error(..)) where

import Prelude

import qualified Data.Map as M
import Data.Ratio

import Control.Monad
import Control.Monad.Except

import qualified Sodium.Pascal.Tokenize as T
import Sodium.Pascal.Program

import qualified Text.Parsec as P

}

%partial parser
%tokentype { T.Token   }
%error     { parseErr  }

%monad     { P.Parsec String ()  }
%lexer     { T.tokenCC } { T.EOF }

%token
    var      { T.KwVar   }
    begin    { T.KwBegin }
    end      { T.KwEnd   }
    for      { T.KwFor   }
    to       { T.KwTo    }
    do       { T.KwDo    }
    function { T.KwFunction }
    procedure{ T.KwProcedure}
    true     { T.KwTrue  }
    false    { T.KwFalse }
    and      { T.KwAnd   }
    or       { T.KwOr    }
    xor      { T.KwXor   }
    not      { T.KwNot   }
    div      { T.KwDiv   }
    mod      { T.KwMod   }
    if       { T.KwIf    }
    then     { T.KwThen  }
    else     { T.KwElse  }
    case     { T.KwCase  }
    of       { T.KwOf    }
    array    { T.KwArray }
    '('      { T.LParen  }
    ')'      { T.RParen  }
    ';'      { T.Semicolon }
    ','      { T.Comma     }
    '.'      { T.Dot       }
    '..'     { T.DoubleDot }
    '+'      { T.Plus      }
    '-'      { T.Minus     }
    ':='     { T.Assign    }
    '*'      { T.Asterisk  }
    '/'      { T.Slash     }
    ':'      { T.Colon     }
    '='      { T.EqSign    }
    '<'      { T.Suck      }
    '>'      { T.Blow      }
    '<>'     { T.SuckBlow  }
    '<='     { T.SuckEq    }
    '>='     { T.BlowEq    }
    '['      { T.LSqBrace  }
    ']'      { T.RSqBrace  }
    name     { T.Name $$   }
    inumber  { T.INumber $$ }
    fnumber  { T.FNumber $$ }
    quote    { T.Quote $$  }
    unknown  { T.Unknown _ }

%nonassoc '<' '>' '<>' '<=' '>=' '='
%left     '+' '-' or  xor
%left     '*' '/' div mod and
%left     NEG POS
%left     not

%%

Program : Decls Vars Body '.' { Program (reverse $1) $2 $3 }

Decls :            {      [] }
      | Decls Decl { $2 : $1 }

Decl : Func { $1 }
     | Proc { $1 }

Vars  :              { M.empty }
      | var VarDecls { M.fromList $2 }

VarDecls :                  {       [] }
         | VarDecls VarDecl { $2 ++ $1 }

VarDecl : VarNames ':' Type ';' { map (\name -> (,) name $3) $1 }

VarNames :              name { $1 : [] }
         | VarNames ',' name { $3 : $1 }


Func : function  name         Params ':' Type ';' Vars Body ';'
     { Func      $2  (FuncSig $3     (Just $5) )  $7   $8    }

Proc : procedure name         Params          ';' Vars Body ';'
     { Func      $2  (FuncSig $3     Nothing   )  $5   $6    }

Params :                    { [] }
       | '('            ')' { [] }
       | '(' ParamDecls ')' { $2 }

ParamDecls : ParamDecl                { $1 ++ [] }
           | ParamDecl ';' ParamDecls { $1 ++ $3 }

ParamDecl : ParamIsVar ParamNames ':' Type
          { map (\name -> ParamDecl name ($1, $4)) (reverse $2) }

ParamIsVar :     { ByValue     }
ParamIsVar : var { ByReference }

ParamNames :                name { $1 : [] }
           | ParamNames ',' name { $3 : $1 }

Body : begin Statements end { reverse $2 }

Statements :                           {      [] }
           |                Statement  { $1 : [] }
           | Statements ';' Statement_ { $3 : $1 }

Statement_ : Statement  { $1 }
           |            { BodyStatement [] }

Statement  : AssignStatement  { $1 }
           | ExecuteStatement { $1 }
           | ForStatement     { $1 }
           | IfStatement      { $1 }
           | CaseStatement    { $1 }
           | Body             { BodyStatement $1 }


AssignStatement  : name ':=' Expression { Assign $1 $3 }

ExecuteStatement : name           { Execute $1 [] }
                 | name Arguments { Execute $1 $2 }

ForStatement : for      name ':=' Expression to Expression do Statement_
             { ForCycle $2        $4            $6            $8 }

IfStatement : if       Expression ThenClause ElseClause
            { IfBranch $2         $3         $4 }

ThenClause : then Statement_ { $2 }
ElseClause :                 { Nothing }
           | else Statement_ { Just $2 }

CaseStatement : case       Expression of CaseClauses  CaseElseClause end
              { CaseBranch $2            (reverse $4) $5 }

CaseElseClause :                 { Nothing }
               | else Statements { Just (BodyStatement $2) }

CaseClauses :                        {      [] }
            | CaseClauses CaseClause { $2 : $1 }

CaseClause : Ranges ':' Statement_ ';' { (reverse $1, $3) }

Ranges : Range            { $1 : [] }
       | Ranges ',' Range { $3 : $1 }

Range :                  Expression_ { Left $1 }
      | Expression_ '..' Expression_ { Right ($1, $3) }

Expression : Expression '<' Expression { binary OpLess   $1 $3 }
           | Expression '>' Expression { binary OpMore   $1 $3 }
           | Expression '=' Expression { binary OpEquals $1 $3 }
           | Expression '>=' Expression { binary OpMoreEquals $1 $3 }
           | Expression '<=' Expression { binary OpLessEquals $1 $3 }
           | Expression '<>' Expression { binary OpNotEquals $1 $3 }
           | Expression_ { $1 }

Expression_ : Expression '+' Expression { binary OpAdd      $1 $3 }
            | Expression '-' Expression { binary OpSubtract $1 $3 }
            | Expression  or Expression { binary OpOr       $1 $3 }
            | Expression xor Expression { binary OpXor      $1 $3 }

            | Expression '*' Expression { binary OpMultiply $1 $3 }
            | Expression '/' Expression { binary OpDivide   $1 $3 }
            | Expression div Expression { binary OpDiv      $1 $3 }
            | Expression mod Expression { binary OpMod      $1 $3 }
            | Expression and Expression { binary OpAnd      $1 $3 }

            | '-' Expression %prec NEG  { Call (Left OpNegate) [$2] }
            | '+' Expression %prec POS  { Call (Left OpPlus)   [$2] }
            | not Expression            { Call (Left OpNot)    [$2] }

            | '(' Expression ')' { $2 }
            |     Atom           { $1 }

            | Call { $1 }

Atom : name  { Access $1 }
     | true  { Primary (LitBool True)  }
     | false { Primary (LitBool False) }
     | inumber { Primary (LitInt  $1) }
     | fnumber { Primary (LitReal $1) }
     | quote   { match_quote $1 }

Call : name Arguments { Call (Right $1) $2 }

Type : name { match_type $1 }
     | array of Type { TypeArray $3 }

Arguments  : '(' Arguments_ ')' { reverse $2 }
Arguments_ :                           {      [] }
           |                Expression { $1 : [] }
           | Arguments_ ',' Expression { $3 : $1 }

{
parseErr _ = mzero

binary op x y = Call (Left op) [x, y]

class Error e where
    errorParse :: P.ParseError -> e

parse :: (Error e, MonadError e m) => String -> m Program
parse s = case P.parse parser "" s of
    Left  e -> throwError (errorParse e)
    Right a -> return a

match_quote [c] = Primary (LitChar c)
match_quote cs  = Primary (LitStr cs)

match_type t = case t of
    "integer" -> TypeInteger
    "longint" -> TypeInteger
    "real"    -> TypeReal
    "boolean" -> TypeBoolean
    "string"  -> TypeString
    "char"    -> TypeChar
    _         -> TypeCustom t
}
