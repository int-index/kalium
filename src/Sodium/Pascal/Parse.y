{
module Sodium.Pascal.Parse (parse, tokenize) where

import Control.Monad

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
    if       { T.KwIf    }
    then     { T.KwThen  }
    else     { T.KwElse  }
    case     { T.KwCase  }
    of       { T.KwOf    }
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
    '['      { T.LSqBrace  }
    ']'      { T.RSqBrace  }
    name     { T.Name $$   }
    inumber  { T.INumber _    }
    fnumber  { T.FNumber _ _  }
    enumber  { T.ENumber _ _ _ _ }
    quote    { T.Quote $$  }
    unknown  { T.Unknown _ }

%nonassoc '<' '>' '='
%left     '+' '-' or
%left     '*' '/' and
%left     NEG POS

%%

Program : Decls Vars Body '.' { Program (reverse $1) $2 $3 }

Decls :            {      [] }
      | Decls Decl { $2 : $1 }

Decl : Func { $1 }
     | Proc { $1 }

Vars  :              { [] }
      | var VarDecls { $2 }

VarDecls :                  {      [] }
         | VarDecls VarDecl { $2 : $1 }

VarDecl : VarNames ':' Type ';' { VarDecl $1 $3 }

VarNames :              name { $1 : [] }
         | VarNames ',' name { $3 : $1 }


Func : function  name Params ':' Type ';' Vars Body ';'
     { Func      $2   $3     (Just $5)    $7   $8 }

Proc : procedure name Params          ';' Vars Body ';'
     { Func      $2   $3     Nothing      $5   $6 }

Params :                    { [] }
       | '('            ')' { [] }
       | '(' ParamDecls ')' { $2 }

ParamDecls : ParamDecl                { $1 : [] }
           | ParamDecl ';' ParamDecls { $1 : $3 }

ParamDecl : ParamIsVar ParamNames ':' Type { ParamDecl (reverse $2) $1 $4 }

ParamIsVar :     { False }
ParamIsVar : var { True  }

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

Range :                  Expression_ { $1 }
      | Expression_ '..' Expression_ { Binary OpRange $1 $3 }

Expression : Expression '<' Expression { Binary OpLess   $1 $3 }
           | Expression '>' Expression { Binary OpMore   $1 $3 }
           | Expression '=' Expression { Binary OpEquals $1 $3 }
           | Expression_ { $1 }

Expression_ : Expression '+' Expression { Binary OpAdd      $1 $3 }
            | Expression '-' Expression { Binary OpSubtract $1 $3 }
            | Expression  or Expression { Binary OpOr       $1 $3 }

            | Expression '*' Expression { Binary OpMultiply $1 $3 }
            | Expression '/' Expression { Binary OpDivide   $1 $3 }
            | Expression and Expression { Binary OpAnd      $1 $3 }

            | '-' Expression %prec NEG  { Unary UOpNegate $2 }
            | '+' Expression %prec POS  { Unary UOpPlus   $2 }

            | '(' Expression ')' { $2 }
            |     Atom           { $1 }

            | Call { $1 }

Atom : name  { Access $1 }
     | true  { BTrue     }
     | false { BFalse    }
     | inumber { match_inumber $1 }
     | fnumber { match_fnumber $1 }
     | enumber { match_enumber $1 }
     | quote   { Quote $1 }

Call : name Arguments { Call $1 $2 }

Type : name { match_type $1 }

Arguments  : '(' Arguments_ ')' { reverse $2 }
Arguments_ :                           {      [] }
           |                Expression { $1 : [] }
           | Arguments_ ',' Expression { $3 : $1 }

{
parseErr _ = mzero

parse = either (error.show) id . P.parse parser ""

match_inumber (T.INumber i      ) = INumber i
match_fnumber (T.FNumber i f    ) = FNumber i f
match_enumber (T.ENumber i f s e) = ENumber i f s e

match_type t = case t of
    "integer" -> PasInteger
    "longint" -> PasLongInt
    "real"    -> PasReal
    "boolean" -> PasBoolean
    "string"  -> PasString
    _         -> PasType t

tokenize :: String -> Either P.ParseError [T.Token]
tokenize = P.parse tokenizer "" where
    tokenizer = T.tokenCC cont
    cont T.EOF = return []
    cont token = (token:) `fmap` tokenizer
}
