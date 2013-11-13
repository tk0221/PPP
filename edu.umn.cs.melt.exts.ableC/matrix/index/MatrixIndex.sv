grammar edu:umn:cs:melt:exts:ableC:matrix:index;

imports silver:langutil only ast, pp ; --, errors, err, wrn;
imports silver:langutil:pp ;

imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax;

marking terminal Index_t              'index' lexer classes {Ckeyword};
terminal IndexMarker_t                '=>';

concrete productions top::PostfixExpr_c
| 'index' '(' m::Identifier_t '=>' args::ArgumentExprList_c ')'
     { top.ast = matrixIndex( m.lexeme, args.ast, location=top.location ); }


{-
 - Indexes a matrix "m" through its __data field with its __dimSize field.
 - matrixIndex("m", [1,2]) forwards to
 - 1*m.__dimSize[1] + 2
 -
 - matrixIndex("m", [1,3,5]) forwards to
 - 1*m.__dimSize[1]*m.__dimSize[2] + 3*m.__dimSize[2] + 5
 - 
 - Assumes that "m" has length(is) dimensions
 -}
abstract production matrixIndex
top::Expr ::= m::String is::[Expr]
{
  --top.pp = concat([ text(m), braces( braces(
  --                  ppImplode( cat( comma(), space() ), map( (.pp), is ) ) ) ) ]);

  -- e.__data[i*.... + j]

  forwards to
    arraySubscriptExpr(
      memberExpr(
        declRefExpr(name(m,location=top.location),location=top.location),
        false,
        name("__data",location=top.location),location=top.location),
      generateIndexArith(m,is),location=top.location);
}


{-
 - Uses generateMulUpDims to generate the arithmetic behind the indexing.
 - For example, generateIndexArith("m",[1,3],2) generates
 - 1 * m.__dimSize[1] + 3
 -}
function generateIndexArith
Expr ::= m::String is::[Expr]
{
  return generateIndexArithHelper(m,is,length(is));
}
function generateIndexArithHelper
Expr ::= m::String is::[Expr] max::Integer
{
  return if length(is) > 1
         then binaryOpExpr(
                binaryOpExpr(
                  head(is),
                  numOp(
                    mulOp(location=head(is).location),location=head(is).location),
                  generateMulUpDims(m, max-length(is)+1, max),location=head(is).location),
                numOp(
                  addOp(location=head(is).location),location=head(is).location),
                generateIndexArithHelper(m,tail(is),max),location=head(is).location)
          else head(is);
}


{-
 - Generates expression which would compute the product of 
 - the indices of m's __dimSize array from i up to and not including
 - max. For example, generateMulUpDims("m",2,4) generates
 - m.__dimSize[2] * m.__dimSize[3]
 -}
function generateMulUpDims
Expr ::= m::String i::Integer max::Integer
{
  return 
    if i < max-1
    then binaryOpExpr(
           arraySubscriptExpr(
             memberExpr(
               declRefExpr(
                 name(m,location=builtIn1()),location=builtIn1()),
               false,
               name("__dimSize",location=builtIn1()),location=builtIn1()),
             integerLiteral(toString(i),location=builtIn1()),location=builtIn1()),
           numOp(
             mulOp(location=builtIn1()),location=builtIn1()),
           generateMulUpDims(m,i+1,max),location=builtIn1())
    else arraySubscriptExpr(
           memberExpr(
             declRefExpr(
               name(m,location=builtIn1()),location=builtIn1()),
             false,
             name("__dimSize",location=builtIn1()),location=builtIn1()),
           integerLiteral(toString(i),location=builtIn1()),location=builtIn1());
}


{-
 - New location for expressions which don't have real locations
 -}
abstract production builtIn1
top::Location ::=
{
  forwards to loc("Built In", 0, 0, 0, 0, 0, 0);
}
