grammar lambda_calculus:host:abstractsyntax ;

nonterminal Root ;
nonterminal Expr ;
nonterminal Type ;

synthesized attribute pp :: String occurs on Root, Expr, Type ;
synthesized attribute errors :: [String] with ++ ;
attribute errors occurs on Root, Expr, Type ;

abstract production root
r::Root ::= e::Expr
{
  r.pp = e.pp ;
  r.errors := [ ] ;
}

abstract production application
e::Expr ::= f::Expr a::Expr
{ 
  e.pp = "( " ++ f.pp ++ " ) ( " ++ a.pp ++ " )" ;
  e.errors := [ ] ;
}

abstract production lambda
e::Expr ::= n::String t::Type b::Expr
{ 
  e.pp = "( \\ " ++ n ++ " : " ++ t.pp ++ " . " ++ b.pp ++ " )" ;
  e.errors := [ ] ;
}

abstract production intConst
e::Expr ::= v::Integer
{ 
  e.pp = toString (v) ;
  e.errors := [ ] ;
}

abstract production name
e::Expr ::= n::String
{ 
  e.pp = n ;
  e.errors := [ ] ;
}

abstract production addition
e::Expr ::= l::Expr r::Expr
{ 
  e.pp = "( " ++ l.pp ++ " + " ++ r.pp ++ " )" ;
  e.errors := [ ] ;
}

abstract production multiplication
e::Expr ::= l::Expr r::Expr
{
  e.pp = "( " ++ l.pp ++ " * " ++ r.pp ++ " )" ;
  e.errors := [ ] ;
}

abstract production functionType
t::Type ::= inType::Type outType::Type
{
  t.pp = "( " ++ inType.pp ++ " -> " ++ outType.pp ++ " )" ;
  t.errors := [ ] ;
}

abstract production integerType
t::Type ::=
{
  t.pp = "Integer" ;
  t.errors := [ ] ;
}

abstract production errorType
t::Type ::=
{
  t.pp = "Error" ;
  t.errors := [ ] ;
}
