grammar lambda_calculus:host:abstractsyntax ;

synthesized attribute type :: Type occurs on Root, Expr ;
inherited attribute env :: [ Pair<String Type> ] occurs on Expr ;

aspect production root
r::Root ::= e::Expr
{
  r.type = e.type ;
  e.env = [ ] ;
  r.errors <- e.errors ;
}

aspect production application
e::Expr ::= f::Expr a::Expr
{ 
  e.type = case f.type of
           | functionType (i, o) -> if equalsType (i, a.type) then o else errorType () 
           | _ -> errorType () 
           end ;

  e.errors <- if null(f.errors) && null(a.errors)
              then case f.type of
                   | functionType (i, o) -> if equalsType (i, a.type) then [ ] else [ errMsg1 ]
                   | _ -> [ errMsg2 ]
                   end 
              else [ ] ;
  e.errors <- f.errors ;
  e.errors <- a.errors ;

  local attribute errMsg1 :: String 
    = "The expression \"" ++ f.pp ++ "\" in \"" ++ e.pp ++ 
      "\" should have input type of \"" ++ a.type.pp ++ "\"\n" ;
  local attribute errMsg2 :: String 
    = "The expression \"" ++ f.pp ++ "\" in \"" ++ e.pp ++ 
      "\" should be a function type, instead it is \"" ++ 
      f.type.pp ++ "\"\n" ;

  f.env = e.env ;
  a.env = e.env ;  
}

aspect production lambda
e::Expr ::= n::String t::Type b::Expr
{ 
  b.env = [pair(n,t)] ++ e.env ;
  e.type = functionType ( t, b.type ) ;
  e.errors <- b.errors ;
}

aspect production addition
e::Expr ::= l::Expr r::Expr
{ e.type = case l.type, r.type of
           | integerType(), integerType() -> integerType() 
           | _, _ -> errorType() 
           end ;
  e.errors <- case l.type of 
              | integerType() -> [ ] 
              | _ -> [ "The expression \"" ++ l.pp ++ "\" in \"" ++ e.pp ++ 
                       "\" should be an integer type, instead it is \"" ++ 
                       l.type.pp ++ "\"\n" ]
              end ;

  e.errors <- case r.type of 
              | integerType() -> [ ] 
              | _ -> [ "The expression \"" ++ r.pp ++ "\" in \"" ++ e.pp ++ 
                       "\" should be an integer type, instead it is \"" ++ 
                       r.type.pp ++ "\"\n" ]
              end ;
  e.errors <- l.errors ;
  e.errors <- r.errors ;

  l.env = e.env ; 
  r.env = e.env ;
}

aspect production multiplication
e::Expr ::= l::Expr r::Expr
{ e.type = case l.type, r.type of
           | integerType(), integerType() -> integerType() 
           | _, _ -> errorType() 
           end ;
  e.errors <- case l.type of 
              | integerType() -> [ ] 
              | _ -> [ "The expression \"" ++ l.pp ++ "\" in \"" ++ e.pp ++ 
                       "\" should be an integer type, instead it is \"" ++ 
                       l.type.pp ++ "\"\n" ]
              end ;

  e.errors <- case r.type of 
              | integerType() -> [ ] 
              | _ -> [ "The expression \"" ++ r.pp ++ "\" in \"" ++ e.pp ++ 
                       "\" should be an integer type, instead it is \"" ++ 
                       r.type.pp ++ "\"\n" ]
              end ;

  e.errors <- l.errors ;
  e.errors <- r.errors ;

  l.env = e.env ; 
  r.env = e.env ;
}

aspect production intConst
e::Expr ::= v::Integer
{ 
  e.type = integerType() ;
}

aspect production name
e::Expr ::= n::String
{ 
  e.type = case lookupBy (stringEq, n, e.env) of
           | just(t) -> t
           | nothing() -> errorType() 
           end ;
  e.errors <- case lookupBy (stringEq, n, e.env) of
              | just(t) -> [ ]
              | nothing() -> [ "Name \"" ++ n ++ "\" is not declared.\n" ]
              end ;
}

