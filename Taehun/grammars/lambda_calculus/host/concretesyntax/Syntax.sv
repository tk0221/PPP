grammar lambda_calculus:host:concretesyntax ;

import lambda_calculus:host:abstractsyntax as abs ;

--------------------
-- Lexical Syntax --
--------------------

ignore terminal WS_t /[\ \t\n]+/ ;
ignore terminal LineComment_t /\/\/.*/ ;

terminal Dot_t        '.' ;
terminal Lambda_t     '\' ; 
terminal Arrow_t      '->'   precedence = 5, association = right ;
terminal LeftParen_t  '(' ;
terminal RightParen_t ')' ;
terminal Colon_t      ':' ;
terminal Name_t       /[a-zA-Z][a-zA-Z0-9_]*/ ;

terminal IntConst_t /[0-9]+/ ;

terminal Star_t     '*'  precedence = 10, association = left ;
terminal Plus_t     '+'  precedence =  8, association = left ;

terminal Integer_t 'Integer' dominates { Name_t } ;

synthesized attribute pp :: String ;

-------------------------
-- Context free syntax --
-------------------------

-- Root --
----------
nonterminal Root with pp, ast_Root ;

synthesized attribute ast_Root :: abs:Root ;

concrete production root
r::Root ::= l::LamExpr
{
 r.pp = l.pp ;
 r.ast_Root = abs:root ( l.ast_Expr ) ;
}


-- Expr --
----------
nonterminal LamExpr with pp, ast_Expr ;

synthesized attribute ast_Expr :: abs:Expr ;

concrete productions l::LamExpr
| lambda::Lambda_t n::Name_t ':' t::Type '.' b::LamExpr
  {
    l.pp = "( \\ " ++ n.lexeme ++ " : " ++ t.pp ++ " . " ++ b.pp ++ " )" ;
    l.ast_Expr = abs:lambda ( n.lexeme, t.ast_Type, b.ast_Expr ) ; 
  }
| e::Expr
  {
    l.pp = e.pp ;
    l.ast_Expr = e.ast_Expr ;
  }


nonterminal Expr with pp, ast_Expr ;
concrete productions e::Expr 
| l::Expr '+' r::Expr
  {
    e.pp = "( " ++ l.pp ++ " + " ++ r.pp ++ " )" ;
    e.ast_Expr = abs:addition (l.ast_Expr, r.ast_Expr) ;
  }
| l::Expr '*' r::Expr
  {
    e.pp = "( " ++ l.pp ++ " * " ++ r.pp ++ " )" ;
    e.ast_Expr = abs:multiplication (l.ast_Expr, r.ast_Expr) ;
  }

{- You may add additional productions like the two above here.  These
   will define subtraction, division, and the logical operators (||,
   &&) and relational operators (==, <, >, <=, etc).
-}


| t::AppTerm
  {
    e.pp = t.pp ;
    e.ast_Expr = t.ast_Expr ;
  }


nonterminal AppTerm with pp, ast_Expr ;
concrete productions t::AppTerm
| f::AppTerm a::ATerm
  {
    t.pp = "( " ++ f.pp ++ " ) ( " ++ a.pp ++ " )" ;
    t.ast_Expr = abs:application( f.ast_Expr, a.ast_Expr ) ;
  }
| a::ATerm
  {
    t.pp = a.pp ;
    t.ast_Expr = a.ast_Expr ;
  }

nonterminal ATerm with pp, ast_Expr ;
concrete productions a::ATerm
| '(' nested::LamExpr ')'
  { 
    a.pp = "( " ++ nested.pp ++ " )" ;
    a.ast_Expr = nested.ast_Expr ;
  }
| i::IntConst_t
  {
    a.pp = i.lexeme ;
    a.ast_Expr = abs:intConst( toInt(i.lexeme) ) ;
  }
| n::Name_t
  {
    a.pp = n.lexeme ;
    a.ast_Expr = abs:name (n.lexeme) ;
  }

{- New constants, for boolean values, empty lists, list literals, and
   operations (such as head, tail, etc) should be added above as ATerm
   productions.  
-}



nonterminal Type with pp, ast_Type ;
synthesized attribute ast_Type :: abs:Type ;

concrete production functionType
t::Type ::= inType::Type '->' outType::Type
{
  t.pp = "( " ++ inType.pp ++ " -> " ++ outType.pp ++ " )" ;
  t.ast_Type = abs:functionType ( inType.ast_Type, outType.ast_Type ) ;
}

concrete production integerType
t::Type ::= 'Integer'
{
 t.pp = "Integer" ;
 t.ast_Type = abs:integerType() ;
}


{- You may also want to add new types, which would go above.
 -}
