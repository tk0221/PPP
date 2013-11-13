
imports edu:umn:cs:melt:ableC:abstractsyntax;
imports edu:umn:cs:melt:ableC:concretesyntax;

imports silver:langutil;
imports silver:langutil:pp with implode as ppImplode;

marking terminal MatrixMatrixAdd_t            '+m';

concrete productions top::AdditiveOp_c
| '+m'
    { top.ast = matrixMatrixAdd(top.leftExpr,
                                top.rightExpr,
                                location=$1.location); }


abstract production matrixMatrixAdd
top::Expr ::= l::Expr r::Expr
{
  --top.pp = concat([ l.pp, space(), text("+m"), space(), r.pp ]);

  forwards to
    callExpr(
      declRefExpr(
        name("matrixMatrixAdd",location=top.location),location=top.location),
      consExpr(
        l,
        consExpr(
          r,
          nilExpr())),location=top.location);
}
