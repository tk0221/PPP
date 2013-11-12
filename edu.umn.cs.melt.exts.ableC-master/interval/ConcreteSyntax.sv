
imports edu:umn:cs:melt:ableC:abstractsyntax;
imports edu:umn:cs:melt:ableC:concretesyntax;
imports silver:langutil;
imports silver:langutil:pp with implode as ppImplode;

--marking terminal Interval_t     'Interval' lexer classes {Ckeyword};
terminal DoubleColon_t  '::';


{-concrete productions top::TypeName_c
| 'Interval'
    { top.ast = intervalTypeExpr(); }
-}
concrete productions top::ConditionalExpr_c
| l::LogicalOrExpr_c '::' r::LogicalOrExpr_c
    { top.ast = intervalExpr( l.ast, r.ast, location=top.location ); }


