grammar edu:umn:cs:melt:exts:ableC:matrix:intervalIndex;

imports silver:langutil only ast, pp ; --, errors, err, wrn;
imports silver:langutil:pp ;

imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax;


marking terminal IntervalIndex_t      'intervalIndex' lexer classes {Ckeyword};
terminal IndexMarker_t                '=>';

concrete productions top::PostfixExpr_c
| 'intervalIndex' '(' m::Identifier_t '=>' args::ArgumentExprList_c ')'
     { top.ast = matrixIntervalIndex( m.lexeme, args.ast, location=top.location ); }


{-
 - intervalIndex( m => 2::5, 2:4 )
 - Assumes every index is an interval
 -}
abstract production matrixIntervalIndex
top::Expr ::= m::String is::[Expr]
{
  --TODO Ensure each is an interval
  --TODO Ensure each interval's high doesn't exceed m's size in that dimension

  {-
   - struct Matrix_Int out;
   - out.__numDimensions = length(is);
   - out.__dimSize = (int*) malloc( sizeof(int) * out.__numDimensions );
   - Interval interval0;
   - Interval interval1;
   - ...
   - 
   - interval0 = is[0];
   - interval1 = is[1];
   - ...
   - 
   - out.__dimSize[0] = interval0.high - interval0.low;
   - out.__dimSize[1] = interval1.high - interval1.low;
   - ...
   - 
   - out.__data = (int*) malloc( sizeof(int) * out.__dimSize[0] * ... );
   - 
   - int i;
   - for (i = is[0].low; i < is[0].high; i++) {
   -   for (j = is[1].low; j < is[1].high; j++) {
   -     ...
   -       out.__data[(i - is[0].low)*out.__dimSize[1]*... + (j - is[1].low)*out.__dimSize[2]*... + ...] = m.__data[i*out.__dimSize[1]*... + j*out.__dimSize[2]*... + ...];
   -     ...
   -   }
   - }
   - 
   - return out;
   -}

  forwards to stmtExpr( foldr( compoundStmt, nullStmt(),
                        [ outDecl, setNumDimensions, mallocDimSize, 
                          declIntervalVars, assignIntervalVars, fillDimSize,
                          declIndexVars, mallocData, fillData, returnOut ] ), location=top.location );

  -- struct Matrix_Int out;
  local outDecl::Stmt = 
    declStmt(
      variableDecls(
        [],
        tagReferenceTypeExpr(
          [],
          structSEU(),
          name("Matrix_Int"{-findMatrixStructName(t.typerep)-}, location=top.location)),
        consDeclarator(
          declarator(
            name("out", location=top.location),
            baseTypeExpr(),
            nothingInitializer()),
          nilDeclarator())));

  -- out.__numDimensions = length(is);
  local setNumDimensions::Stmt =
    exprStmt(
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name("out",location=top.location),location=top.location), 
          false, 
          name("__numDimensions",location=top.location),location=top.location), 
        assignOp(
          eqOp(location=top.location),location=top.location), 
        integerLiteral(toString(length(is)),location=top.location),location=top.location));

  -- out.__dimSize = (int*) malloc( sizeof(int) * out.__numDimensions );
  local mallocDimSize::Stmt =
    exprStmt(
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name("out",location=top.location),location=top.location),
          false, 
          name("__dimSize",location=top.location),location=top.location),
        assignOp(
          eqOp(location=top.location),location=top.location), 
        explicitCastExpr(
          typeName(
            directTypeExpr(
              builtinType([], 
                signedType(
                  intType()))),
            pointerTypeExpr([],
              baseTypeExpr())), 
          callExpr(
            declRefExpr(
              name("malloc",location=top.location),location=top.location), 
            consExpr(
              binaryOpExpr(
                unaryExprOrTypeTraitExpr(
                  sizeofOp(location=top.location), 
                  typeNameExpr(
                    typeName(
                      directTypeExpr(
                        builtinType([], 
                          signedType(
                            intType()))),
                      baseTypeExpr())),location=top.location),
                numOp(
                  mulOp(location=top.location),location=top.location),
                memberExpr(
                  declRefExpr(
                    name("out",location=top.location),location=top.location), 
                  false, 
                  name("__numDimensions",location=top.location),location=top.location),location=top.location), 
              nilExpr()),location=top.location),location=top.location),location=top.location));

  -- ["interval0", ...]
  local intervalVars::[String] = generateIntervalVars( length(is) );

  -- Interval interval0; Interval interval1; ...
  local declIntervalVars::Stmt = generateIntervalVarDecls( intervalVars );

  -- interval0 = is[0]; interval1 = is[1]; ...
  local assignIntervalVars::Stmt = generateIntervalAssignment( intervalVars, is );

  -- [interval0.high - interval0.low, ...]
  local intervalDifferences::[Expr] = generateIntervalDifferences( intervalVars );

  -- out.__dimSize[0] = is[0].high - is[0].low;
  -- out.__dimSize[1] = is[1].high - is[1].low;
  -- ...
  local fillDimSize::Stmt = foldr1( compoundStmt, fillDimSizeFun("out", 0, intervalDifferences) );

  -- [i0, i1, ...]
  local indexVars::[String] = generateIndexVars( length(is) );

  local indexVarExprs::[Expr] = map( convertStringToDeclVarExpr, indexVars );

  local indexVarsPlusIntervalLows::[Expr] = map( convertIndexVarIntervalAddLow, zip( indexVars, intervalVars ) );

  -- int i0;
  -- int i1;
  -- ...
  local declIndexVars::Stmt = generateIndexVarDecls( indexVars );

  -- out.__data = (int*) malloc( sizeof(int) * out.__dimSize[0] * ... );
  local mallocData::Stmt =
    exprStmt(
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name("out",location=builtIn()),location=builtIn()), 
          false, 
          name("__data",location=builtIn()),location=builtIn()),
        assignOp(
          eqOp(location=builtIn()),location=builtIn()), 
        explicitCastExpr(
          typeName(
            directTypeExpr(
              builtinType([], 
                signedType(
                  intType()))), 
            pointerTypeExpr([], baseTypeExpr())),
          callExpr(
            declRefExpr(
              name("malloc",location=builtIn()),location=builtIn()), 
            consExpr(
              binaryOpExpr(
                unaryExprOrTypeTraitExpr(
                  sizeofOp(location=builtIn()), 
                  typeNameExpr(
                    typeName(
                      directTypeExpr(
                        builtinType([], 
                          signedType(
                            intType()))),
                      baseTypeExpr())),
                  location=builtIn()),
                numOp(
                  mulOp(location=builtIn()),location=builtIn()), 
                generateMulAllDims("out",length(is)),location=builtIn()),
              nilExpr()),location=builtIn()),location=builtIn()),location=builtIn()));


  -- temp0 * out.__dimSize[1] * ... + temp1 * out.__dimSize[2] * ... + tempN
  local innerIndexArith::Expr = generateMatrixArith( indexVarExprs );

  -- (temp0 + interval0.low)*out.__dimSize[1] * ... + 
  -- (temp1 + interval1.low)*out.__dimSize[1] * ... +
  -- ... +
  -- (tempN + intervalN.low)
  local outerIndexArith::Expr = generateMatrixArith( indexVarsPlusIntervalLows );

  local rhsOfAssignment::Expr =
    arraySubscriptExpr(
      memberExpr(
        declRefExpr(
          name(m,location=builtIn()),location=builtIn()),
        false,
        name("__data",location=builtIn()),location=builtIn()),
      outerIndexArith,location=builtIn());

  -- for (i = 0; i < out.__dimSize[0]; i++) {
  --   for (j = 0; j < out.__dimSize[1]; j++) {
  --     ...
  --       out.__data[i*out.__dimSize[1]*... + j*out.__dimSize[2]*... + ...] =
  --         m.__data[(i + is[0].low)*out.__dimSize[1]*... + (j + is[1].low)*out.__dimSize[2]*... + ...];
  --     ...
  --   }
  -- }
  local fillData::Stmt = generateForLoopAssign("out", indexVars, rhsOfAssignment, innerIndexArith );


  local returnOut::Stmt =
    exprStmt(
      declRefExpr(
        name("out",location=top.location),location=top.location));
}

{-
 - generateIntervalDifferences( [interval0, interval1] ) =
 - 
 - [interval0.high - interval0.low, interval1.high - interval1.low]
 -}
function generateIntervalDifferences
[Expr] ::= vs::[String]
{
  return
    if null(vs)
    then []
    else
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name(head(vs),location=builtIn()),location=builtIn()),
          false,
          name("high",location=builtIn()),location=builtIn()),
        numOp(
          subOp(location=builtIn()),location=builtIn()),
        memberExpr(
          declRefExpr(
            name(head(vs),location=builtIn()),location=builtIn()),
          false,
          name("low",location=builtIn()),location=builtIn()),location=builtIn())
      :: generateIntervalDifferences( tail(vs) );
}

{-
 - __dimSize is an array of the size of each dimension of a matrix.
 - This function unfolds the expressions defining the size of each
 - dimension and saves them in in the array
 -
 - n = the name of the array
 - i = the dimension number, counting up on each recursive call
 - arr = the list of expressions for each dimension. The head is
 -       used immediately and the tail is used in the recursive
 -       call.
 -}
function fillDimSizeFun
[Stmt] ::= n::String i::Integer arr::[Expr]
{
  return if null(arr)
         then [nullStmt()]
         else exprStmt(
                binaryOpExpr(
                  unaryOpExpr(
                    dereferenceOp(location=builtIn()), 
                    parenExpr(
                      binaryOpExpr(
                        memberExpr(
                          declRefExpr(
                            name(n,location=builtIn()),location=builtIn()), 
                          false, 
                          name("__dimSize",location=builtIn()),location=builtIn()), 
                        numOp(
                          addOp(location=builtIn()),location=builtIn()), 
                        integerLiteral(toString(i),location=builtIn()),location=builtIn()),location=builtIn()),location=builtIn()), 
                  assignOp(
                    eqOp(location=builtIn()),location=builtIn()), 
                  head(arr),location=builtIn())) ::
              fillDimSizeFun( n, i+1, tail(arr) );
}


function generateIntervalVars
[String] ::= i::Integer
{
  return if i > 0 then ("interval" ++ toString(i)) :: generateIntervalVars( i - 1 )
         else [];
}

function generateIntervalVarDecls
Stmt ::= vs::[String]
{
  return if null(vs)
         then nullStmt()
         else compoundStmt(
                declStmt(
                  variableDecls(
                    [],
                    typedefTypeExpr(
                      [],
                      name("Interval",location=builtIn())), 
                    consDeclarator(
                      declarator(
                        name(head(vs),location=builtIn()),
                        baseTypeExpr(),
                        nothingInitializer()),
                      nilDeclarator()))),
                generateIntervalVarDecls(tail(vs)));
}

function generateIntervalAssignment
Stmt ::= vs::[String] is::[Expr]
{
  return if null(vs) then nullStmt()
         else
    compoundStmt(
      exprStmt(
        binaryOpExpr(
          declRefExpr(
            name(head(vs), location=builtIn()),location=builtIn()),
          assignOp(
            eqOp(location=builtIn()),location=builtIn()),
          head(is),location=builtIn())),
      generateIntervalAssignment(tail(vs),tail(is)));
}

function generateIndexVars
[String] ::= i::Integer
{
  return if i > 0 then ("temp" ++ toString(i)) :: generateIndexVars( i - 1 )
         else [];
}

function convertStringToDeclVarExpr
Expr ::= indexVar::String
{
  return declRefExpr(name(indexVar,location=builtIn()),location=builtIn());
}

function convertIndexVarIntervalAddLow
Expr ::= input::Pair<String String>
{
  return
    binaryOpExpr(
      declRefExpr(
        name(input.fst,location=builtIn()),location=builtIn()),
      numOp(
        addOp(location=builtIn()),location=builtIn()),
      memberExpr(
        declRefExpr(
          name(input.snd,location=builtIn()),location=builtIn()),
        false,
        name("low",location=builtIn()),location=builtIn()),location=builtIn());
}

function generateIndexVarDecls
Stmt ::= vs::[String]
{
  return if null(vs)
         then nullStmt()
         else compoundStmt(
                declStmt(
                  variableDecls(
                    [],
                    directTypeExpr(
                      builtinType([],
                        signedType(
                          intType()))),
                    consDeclarator(
                      declarator(
                        name(head(vs),location=builtIn()),
                        baseTypeExpr(),
                        nothingInitializer()),
                      nilDeclarator()))),
                generateIndexVarDecls(tail(vs)));
}

function generateMulAllDims
Expr ::= n::String i::Integer
{
  return 
    if i > 1
    then binaryOpExpr(
           arraySubscriptExpr(
             memberExpr(
               declRefExpr(
                 name(n,location=builtIn()),location=builtIn()),
               false,
               name("__dimSize",location=builtIn()),location=builtIn()),
             integerLiteral(toString(i-1),location=builtIn()),location=builtIn()),
           numOp(
             mulOp(location=builtIn()),location=builtIn()),
           generateMulAllDims(n,i-1),location=builtIn())
    else   arraySubscriptExpr(
             memberExpr(
               declRefExpr(
                 name(n,location=builtIn()),location=builtIn()),
               false,
               name("__dimSize",location=builtIn()),location=builtIn()),
             integerLiteral(toString(i-1),location=builtIn()),location=builtIn());
}

function generateMatrixArith
Expr ::= vs::[Expr]
{
  return generateMatrixArithHelper( vs, length(vs) );
}

function generateMatrixArithHelper
Expr ::= vs::[Expr] max::Integer
{
  return if null(vs) then error("Compiler error: Empty list of vars in generateInnerMatrixArith")
         else if length(vs) == 1 then head(vs)
              else
    -- length(vs) > 1
    binaryOpExpr(
      binaryOpExpr(
        head(vs),
        numOp(
          mulOp(location=builtIn()),location=builtIn()),
        generateMulUpDims(max-length(vs)+1, max),location=builtIn()),
      numOp(
        addOp(location=builtIn()),location=builtIn()),
      generateMatrixArithHelper( tail(vs), max ),location=builtIn());
}

function generateMulUpDims
Expr ::= min::Integer max::Integer
{
  return if min >= max then error("Compiler error: min >= max in generateMulUpDims")
         else if min == max - 1 then
    arraySubscriptExpr(
      memberExpr(
        declRefExpr(name("out",location=builtIn()),location=builtIn()),
        false,
        name("__dimSize",location=builtIn()),location=builtIn()),
      integerLiteral(toString(min),location=builtIn()),location=builtIn())
         else
    binaryOpExpr(
      arraySubscriptExpr(
        memberExpr(
          declRefExpr(name("out",location=builtIn()),location=builtIn()),
          false,
          name("__dimSize",location=builtIn()),location=builtIn()),
        integerLiteral(toString(min),location=builtIn()),location=builtIn()),
      numOp(
        mulOp(location=builtIn()),location=builtIn()),
      generateMulUpDims(min+1,max),location=builtIn());
}

function generateForLoopAssign
Stmt ::= n::String indexVars::[String] e::Expr index::Expr
{
  return generateForLoopAssignHelper( n, indexVars,
                                      e, index, 0 );

}

function generateForLoopAssignHelper
Stmt ::= n::String indexVars::[String] e::Expr index::Expr i::Integer 
{
  return
    if null(indexVars)
    then warnStmt( [] )
    else if length(indexVars) == 1
    then -- Innermost loop!
      forStmt(
        justExpr(
          binaryOpExpr(
            declRefExpr(
              name(head(indexVars),location=builtIn()),location=builtIn()), 
            assignOp(
              eqOp(location=builtIn()),location=builtIn()), 
            integerLiteral("0",location=builtIn()),location=builtIn())),
        justExpr(
          binaryOpExpr(
            declRefExpr(
              name(head(indexVars),location=builtIn()),location=builtIn()), 
            compareOp(
              ltOp(location=builtIn()),location=builtIn()), 
            arraySubscriptExpr(
              memberExpr(
                declRefExpr(
                  name(n,location=builtIn()),location=builtIn()),
                false,
                name("__dimSize",location=builtIn()),location=builtIn()),
              integerLiteral(toString(i),location=builtIn()),location=builtIn()),location=builtIn())),
        justExpr(
          unaryOpExpr(
            postIncOp(location=builtIn()), 
            declRefExpr(name(head(indexVars),location=builtIn()),location=builtIn()),location=builtIn())),
        exprStmt(
          binaryOpExpr(
            arraySubscriptExpr(
              memberExpr(
                declRefExpr(
                  name(n,location=builtIn()), location=builtIn()),
                false,
                name("__data",location=builtIn()),location=builtIn()),
              index,location=builtIn()),
            assignOp(
              eqOp(location=builtIn()),location=builtIn()), 
            e,location=builtIn())))

         else -- Normal loop
           forStmt(
             justExpr(
               binaryOpExpr(
                 declRefExpr(
                   name(head(indexVars),location=builtIn()),location=builtIn()), 
                 assignOp(
                   eqOp(location=builtIn()),location=builtIn()), 
                 integerLiteral("0",location=builtIn()),location=builtIn())),
             justExpr(
               binaryOpExpr(
                 declRefExpr(
                   name(head(indexVars),location=builtIn()),location=builtIn()), 
                 compareOp(
                   ltOp(location=builtIn()),location=builtIn()), 
                 arraySubscriptExpr(
                   memberExpr(
                     declRefExpr(
                       name(n,location=builtIn()),location=builtIn()),
                     false,
                     name("__dimSize",location=builtIn()),location=builtIn()),
                   integerLiteral(toString(i),location=builtIn()),location=builtIn()),location=builtIn())),
             justExpr(
               unaryOpExpr(
                 postIncOp(location=builtIn()), 
                 declRefExpr(name(head(indexVars),location=builtIn()),location=builtIn()),location=builtIn())),
             compoundStmt(
               generateForLoopAssignHelper(n, tail(indexVars),
                 e, index, i+1),
               nullStmt()));
}

{-
 - New location for expressions which don't have real locations
 -}
abstract production builtIn
top::Location ::=
{
  forwards to loc("Built In", 0, 0, 0, 0, 0, 0);
}

function zip
[Pair<String String>] ::= l::[String] r::[String]
{
  return if length(l) != length(r)
         then error("Compiler error: Zip of differently sized lists")
         else if null(l)
              then []
              else pair(head(l),head(r)) :: zip(tail(l),tail(r));
}