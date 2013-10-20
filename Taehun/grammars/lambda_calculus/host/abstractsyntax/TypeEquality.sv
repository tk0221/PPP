grammar lambda_calculus:host:abstractsyntax ;

{- Checking type equality.

   The function 'equalsType' determines if two types are equal.  
   This way of defining equality makes it possible for other types
   introduced as extensions to define the 'equalsTo' attribute to
   be checked by this function.
 -}
function equalsType
Boolean ::= t1::Type t2::Type
{
 t1.compareTo = t2 ;
 return t1.equalsTo ;
}

synthesized attribute equalsTo :: Boolean occurs on Type ;
inherited attribute compareTo :: Type occurs on Type ;

aspect production functionType
t::Type ::= inType::Type outType::Type
{
  t.equalsTo = case t.compareTo of
               | functionType (i, o) -> equalsType (inType, i) && equalsType (outType, o )
               | _ -> false
               end ;
}

aspect production integerType
t::Type ::=
{
  t.equalsTo = case t.compareTo of
               | integerType() -> true
               | _ -> false
               end ;
}

aspect production errorType
t::Type ::=
{
  t.equalsTo = false ;
}
