grammar lambda_calculus:host:abstractsyntax ;

synthesized attribute haskell :: String occurs on Root, Expr ;

aspect production root
r::Root ::= e::Expr
{
  r.haskell = "module LambdaTranslation where \n\n" ++
              "value = " ++ e.haskell ;

  {- Your first task is to define the translation of our lambad
     calculus language to Haskell.  You can find translations of
     the sample programs in the lambda_calculus/artifacts/host
     directory.  So if you are not very familiar with Haskell these 
     will help quite a bit.

     You will need to provide equations for the haskell attribute for
     all of the productions below. The way it is now, you should be able 
     to run Silver, generate the jar file, and then run that jar file.
     It will generate a file named "LambdaTranslation.hs".  Open this
     haskell interpreter and type "value" to see the value.
  -}
}

aspect production application
e::Expr ::= f::Expr a::Expr
{ 
  e.haskell = "1 + 2" ;
}

aspect production lambda
e::Expr ::= n::String t::Type b::Expr
{ 
  e.haskell = "1 + 2" ;
}

aspect production addition
e::Expr ::= l::Expr r::Expr
{
  e.haskell = "1 + 2" ;
}

aspect production multiplication
e::Expr ::= l::Expr r::Expr
{
  e.haskell = "1 + 2" ;
}

aspect production intConst
e::Expr ::= v::Integer
{ 
  e.haskell = "1 + 2" ;
}

aspect production name
e::Expr ::= n::String
{ 
  e.haskell = "1 + 2" ;
}
