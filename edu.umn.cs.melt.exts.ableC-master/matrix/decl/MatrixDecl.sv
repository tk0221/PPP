grammar edu:umn:cs:melt:exts:ableC:matrix:decl;

imports silver:langutil only ast, pp ; --, errors, err, wrn;
imports silver:langutil:pp ;

imports edu:umn:cs:melt:ableC:abstractsyntax:env;

imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax;


marking terminal Matrix_t             'matrix' lexer classes {Ckeyword};


-- BEGINNING OF FIRST NEW PRODUCTION

{- Concrete production for the matrix type. -}
{-
concrete productions top::TypeName_c
| 'matrix' m::MatrixTypeNameExtension_c
     { top.ast = m.ast; }

nonterminal MatrixTypeNameExtension_c with ast<TypeName>, location;

concrete productions top::MatrixTypeNameExtension_c
| t::TypeSpecifier_c '[' d::DimensionList_c ']'
     { top.ast = 
         matrixTypeExpr(
           case interpretTypeSpecifiers([],t.preTypeSpecifiers) of
           | just(x) -> x
           | nothing() -> error("Unknown preTypeSpecifier in matrix type")
           end, d.dims); }
-}
{- Used to describe dimensions of a new matrix. Currently
 - only finds the number of dimensions and does NOT 
 - track the size of the matrix. Instead, that is stored
 - dynamically within the struct.
 -}{-
nonterminal DimensionList_c;
synthesized attribute dims :: Integer occurs on DimensionList_c;
concrete productions top::DimensionList_c
| e::DecimalConstant_t ',' d::DimensionList_c
     { top.dims = 1 + d.dims; }
| e::DecimalConstant_t
     { top.dims = 1; }
-}


-- BEGINNING OF SECOND NEW PRODUCTION
{-
concrete productions top::BlockItem_c
| 'matrix' m::MatrixDeclExtension_c
     { top.ast = m.ast; }

nonterminal MatrixDeclExtension_c with ast<[Stmt]>, location;
concrete productions top::MatrixDeclExtension_c
| '(' t::TypeName_c ')' n::Identifier_t arr::ArrayLikeDimList_c '=' '{' vs::VarList_c ':' e::Expr_c ';' '}' ';'
     { top.ast = [matrixDeclStmt( t.ast, n.lexeme, arr.dimSizes, vs.vars, e.ast )]; }
-}

concrete productions top::BlockItem_c
| 'matrix' '(' t::TypeName_c ')' n::Identifier_t arr::ArrayLikeDimList_c '=' '{' vs::VarList_c ':' e::Expr_c ';' '}' ';'
     { top.ast = [matrixDeclStmt( t.ast, n.lexeme, arr.dimSizes, vs.vars, e.ast )]; }

nonterminal ArrayLikeDimList_c with location;
synthesized attribute dimSizes :: [Expr] occurs on ArrayLikeDimList_c;
concrete productions top::ArrayLikeDimList_c
| a::ArrayLikeDim_c l::ArrayLikeDimList_c
     { top.dimSizes = a.dimSize :: l.dimSizes; }
| a::ArrayLikeDim_c
     { top.dimSizes = [a.dimSize]; }

nonterminal ArrayLikeDim_c with location;
synthesized attribute dimSize :: Expr occurs on ArrayLikeDim_c;
concrete productions top::ArrayLikeDim_c
| '[' e::Expr_c ']'
     { top.dimSize = e.ast; }

nonterminal VarList_c with location;
synthesized attribute vars :: [String] occurs on VarList_c;
concrete productions top::VarList_c
| n::Identifier_t
     { top.vars = [n.lexeme]; }
| n::Identifier_t ',' v::VarList_c
     { top.vars = n.lexeme :: v.vars; }


abstract production matrixDeclStmt
top::Stmt ::= t::TypeName n::String arr::[Expr] vs::[String] e::Expr
{
  -- matrix int m [5][6] = { i,j : i + j };
  -- Results in a 5x6 matrix m filled with data according to the
  -- cell's location in the matrix

  {-top.pp = concat([ text("matrix"), space(), t.lpp, text(n), t.rpp, space(),
                    concat( map( brackets, map( (.pp), arr ) ) ), space(),
                    braces( concat([ ppImplode( comma(), map( text, vs ) ),
                                     space(), text(":"), space(),
                                     e.pp ]) ) ]); -}

  --TODO Replace builtIn locations
  --TODO Add reliance on t instead of assuming int
  forwards to foldr( compoundStmt, 
                     nullStmt(),
                     [matDecl, setNumDimensions, mallocDimSize, fillDimSize, declIndexVars, mallocData, forLoopMalloc] );

  {- struct Matrix_Int n; -}
  local matDecl :: Stmt =
    declStmt(
      variableDecls(
        [],
        tagReferenceTypeExpr(
          [],
          structSEU(),
          name("Matrix_Int"{-findMatrixStructName(t.typerep)-}, location=builtIn())),
        consDeclarator(
          declarator(
            name(n, location=builtIn()),
            baseTypeExpr(),
            nothingInitializer()),
          nilDeclarator())));

  {- n.__numDimensions = 2; -}
  local setNumDimensions :: Stmt = 
    exprStmt(
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name(n,location=builtIn()),location=builtIn()), 
          false, 
          name("__numDimensions",location=builtIn()),location=builtIn()), 
        assignOp(
          eqOp(location=builtIn()),location=builtIn()), 
        integerLiteral(toString(length(arr)),location=builtIn()),location=builtIn()));

  {- n.__dimSize = (int*) malloc( sizeof(int) * n.__numDimensions ); -}
  local mallocDimSize :: Stmt =
    exprStmt(
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name(n,location=builtIn()),location=builtIn()),
          false, 
          name("__dimSize",location=builtIn()),location=builtIn()),
        assignOp(
          eqOp(location=builtIn()),location=builtIn()), 
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
                      baseTypeExpr())),location=builtIn()),
                numOp(
                  mulOp(location=builtIn()),location=builtIn()),
                memberExpr(
                  declRefExpr(
                    name(n,location=builtIn()),location=builtIn()), 
                  false, 
                  name("__numDimensions",location=builtIn()),location=builtIn()),location=builtIn()), 
              nilExpr()),location=builtIn()),location=builtIn()),location=builtIn()));

  {- *(n.__dimSize + 0) = ...;
   - *(n.__dimSize + 1) = ...;
   - ...
   -}
  local fillDimSize :: Stmt = foldr1( compoundStmt, fillDimSizeFun( n, 0, arr ) );

  {- signed int i0;
   - signed int i1;
   - ...
   -}
  local declIndexVars :: Stmt = declIndexVarsFun( vs );


  {- n.__data = (int*) malloc( sizeof(int) * dim[0] * dim[1] * ... ); -}
  local mallocData :: Stmt = 
    exprStmt(
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name(n,location=builtIn()),location=builtIn()), 
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
                generateMulAllDims(n,length(arr)),location=builtIn()),
              nilExpr()),location=builtIn()),location=builtIn()),location=builtIn()));

  local indexLocation :: Expr = generateIndexLocation( tail(arr), vs );

  -- for (i0 = 0; i0 < n.__dimSize[0]; i0++) {
  --   for (i1 = 0; i1 < n.__dimSize[1]; i1++) {
  --     ....
  --          n.__data[i0*n.__dimSize[1]*...*n.__dimSize[X] + i1*n._dimSize...] = e;
  --     ....
  --   }
  -- }
  local forLoopMalloc :: Stmt = generateForLoopAssign( n, vs, e, indexLocation );
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

{-
 - Functino to generate one declaration for each variable
 - to be used as iterators
 -}
function declIndexVarsFun
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
                declIndexVarsFun(tail(vs)));
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

{- Function to output a list of integers from i to top, inclusive -}
function numList
[Integer] ::= i::Integer top::Integer
{ return if i > top then [] else i :: numList(i+1,top); }

function generateIndexLocation
Expr ::= arr::[Expr] vs::[String]
{
  -- Invariant: length(arr) = length(vs) - 1
  return
    if null(vs)
    then error("Index location with zero vars!")
    else if length(vs) == 1
         then -- Final loop
           declRefExpr(name(head(vs), location=builtIn()),location=builtIn())
         else -- Recursive step
           binaryOpExpr( -- Outer +
             foldr( mulByInput, declRefExpr(name(head(vs), location=builtIn()),location=builtIn()), arr ),
             numOp(
               addOp(location=builtIn()), location=builtIn()),
             generateIndexLocation( tail(arr), tail(vs) ), location=builtIn());
}

function mulByInput
Expr ::= l::Expr r::Expr
{
  return binaryOpExpr( l, numOp( mulOp(location=builtIn()), location=builtIn() ), r, location=builtIn() );
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


{-
 - New location for expressions which don't have real locations
 -}
abstract production builtIn
top::Location ::=
{
  forwards to loc("Built In", 0, 0, 0, 0, 0, 0);
}




abstract production matrixTypeExpr
top::TypeName ::= target::BaseTypeExpr dims::Integer
{
  top.pp = concat([ text("matrix"), space(), target.pp, space(),
                    braces( text( toString(dims) ) )]);

  top.typerep = matrixType( target.typerep, dims );

  target.env = top.env;

{- struct Matrix_Int {
 -   int __numDimensions;
 -   int* __dimSize;
 -   int* __data;
 - }
 -}
  forwards to
    typeName(
      structTypeExpr(
        [],
        structDecl(
          justName(
            name(structName, location=builtIn()) ),
          consStructItem(
            structItem(
              directTypeExpr(builtinType([],signedType(intType()))),
              consStructDeclarator(
                structField(
                  name("__numDimensions", location=builtIn()),
                  baseTypeExpr()),
                nilStructDeclarator())),
            consStructItem(
              structItem(
                directTypeExpr(builtinType([],signedType(intType()))),
                consStructDeclarator(
                  structField(
                    name("__dimSize", location=builtIn()),
                    pointerTypeExpr(
                      [],
                      baseTypeExpr())),
                  nilStructDeclarator())),
              consStructItem(
                structItem(
                  directTypeExpr(builtinType([],signedType(intType()))),
                  consStructDeclarator(
                    structField(
                      name("__data", location=builtIn()),
                      pointerTypeExpr(
                        [],
                        baseTypeExpr())),
                    nilStructDeclarator())),
                nilStructItem() ) ) ) ) ),
      baseTypeExpr());

  --TODO KW Use the function but avoid the warning of exceeding flow type (uses env)
  local attribute structName :: String = "Matrix_Int"; --findMatrixStructName( target.typerep );
}


abstract production matrixType
top::Type ::= target::Type dims::Integer
{
  top.lpp = concat([ text("matrix"), space(), target.lpp, target.rpp,
                    braces( text( toString(dims) ) ) ]);

  top.rpp = notext();

  forwards to
      tagType( 
        [],
        structTagType(
          decorate structDecl(
                     justName(
                       name(structName, location=builtIn()) ),
                     consStructItem(
            structItem(
              directTypeExpr(builtinType([],signedType(intType()))),
              consStructDeclarator(
                structField(
                  name("__numDimensions", location=builtIn()),
                  baseTypeExpr()),
                nilStructDeclarator())),
            consStructItem(
              structItem(
                directTypeExpr(builtinType([],signedType(intType()))),
                consStructDeclarator(
                  structField(
                    name("__dimSize", location=builtIn()),
                    pointerTypeExpr(
                      [],
                      baseTypeExpr())),
                  nilStructDeclarator())),
              consStructItem(
                structItem(
                  directTypeExpr(builtinType([],signedType(intType()))),
                  consStructDeclarator(
                    structField(
                      name("__data", location=builtIn()),
                      pointerTypeExpr(
                        [],
                        baseTypeExpr())),
                    nilStructDeclarator())),
                nilStructItem())))) with {env=emptyEnv();}));

  --TODO KW Use the function but avoid the warning of exceeding flow type (uses env)
  local attribute structName :: String = "Matrix_Int"; --findMatrixStructName( target );
}
