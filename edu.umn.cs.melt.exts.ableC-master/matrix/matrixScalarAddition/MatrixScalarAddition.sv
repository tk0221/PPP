
imports edu:umn:cs:melt:ableC:abstractsyntax;
imports edu:umn:cs:melt:ableC:concretesyntax;

imports silver:langutil;
imports silver:langutil:pp with implode as ppImplode;

marking terminal MatrixScalarAdd_t    '+s';

concrete productions top::AdditiveOp_c
{- Matrix +s Scalar -}
| '+s'
    { top.ast = matrixScalarAdd(top.leftExpr,
                                top.rightExpr,
                                location=$1.location); }


abstract production matrixScalarAdd
top::Expr ::= l::Expr r::Expr
{
  --top.pp = concat([ l.pp, space(), text("+s"), space(), r.pp ]);

  forwards to
    callExpr(
      declRefExpr(
        name("matrixScalarAdd",location=top.location),location=top.location),
      consExpr(
        l,
        consExpr(
          r,
          nilExpr())),location=top.location);
}
