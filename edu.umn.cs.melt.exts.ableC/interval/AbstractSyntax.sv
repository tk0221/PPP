
{-
 - New location for expressions which don't have real locations
 -}
abstract production builtIn
top::Location ::=
{
  forwards to loc("Built In", 0, 0, 0, 0, 0, 0);
}

abstract production intervalTypeExpr
top::TypeName ::=
{
  {-
   - struct Interval {
   -   int low;
   -   int high;
   - }
   -}

  forwards to
    typeName(
      structTypeExpr(
        [],
        structDecl(
          justName(
            name("Interval", location=builtIn())),
          consStructItem(
            structItem(
              directTypeExpr(builtinType([],signedType(intType()))),
              consStructDeclarator(
                structField(
                  name("low", location=builtIn()),
                  baseTypeExpr()),
                nilStructDeclarator())),
            consStructItem(
              structItem(
                directTypeExpr(builtinType([],signedType(intType()))),
                consStructDeclarator(
                  structField(
                    name("high", location=builtIn()),
                    baseTypeExpr()),
                  nilStructDeclarator())),
              nilStructItem())))),
      baseTypeExpr());
}

abstract production intervalExpr
top::Expr ::= l::Expr r::Expr
{
  {-
   - struct Interval i;
   - i.low = l;
   - i.high = r;
   -}
  forwards to stmtExpr( foldr( compoundStmt, nullStmt(), [ iDecl, assignLow, assignHigh, returnI ] ), location=top.location );

  {- Interval i; -}
  local iDecl::Stmt =
    declStmt(
      variableDecls(
        [],
        typedefTypeExpr(
          [],
          name("Interval",location=top.location)), 
        consDeclarator(
          declarator(
            name("i", location=top.location),
            baseTypeExpr(),
            nothingInitializer()),
          nilDeclarator())));

  {- i.low = l; -}
  local assignLow::Stmt = 
    exprStmt(
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name("i",location=top.location),location=top.location), 
          false, 
          name("low",location=top.location),location=top.location), 
        assignOp(
          eqOp(location=top.location),location=top.location), 
        l,location=top.location));

  {- i.high = r; -}
  local assignHigh::Stmt =
    exprStmt(
      binaryOpExpr(
        memberExpr(
          declRefExpr(
            name("i",location=top.location),location=top.location), 
          false, 
          name("high",location=top.location),location=top.location), 
        assignOp(
          eqOp(location=top.location),location=top.location), 
        r,location=top.location));

  {- i; -}
  local returnI::Stmt =
    exprStmt(
      declRefExpr(
        name("i",location=top.location),location=top.location));

}