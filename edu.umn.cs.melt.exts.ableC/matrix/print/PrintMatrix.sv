grammar edu:umn:cs:melt:exts:ableC:matrix:print;

imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax;

imports silver:langutil only ast, pp ; --, errors, err, wrn;
imports silver:langutil:pp ;

marking terminal PrintMatrix_t 'printMatrix' lexer classes {Ckeyword};

concrete productions top::Stmt_c
| 'printMatrix' '(' n::Expr_c ')' ';'
     { top.ast = printMatrix( n.ast ); }


abstract production printMatrix
top::Stmt ::= e::Expr
{
  forwards to
    exprStmt(
      callExpr(
        declRefExpr(
          name("printMatrix",location=e.location),location=e.location),
        consExpr(
          e,
          nilExpr()),location=e.location));
}
