grammar edu:umn:cs:melt:exts:ableC:regex:regexMatching ;

imports edu:umn:cs:melt:ableC:concretesyntax as cnc ;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs ;

imports silver:langutil only ast, pp ; --, errors, err, wrn;
imports silver:langutil:pp ;



-- Regex Matching
marking terminal RegexMatch_t '=~' ;

concrete productions top::cnc:AddMulNoneOp_c
| '=~'
    { top.ast = regexMatch(top.cnc:leftExpr, top.cnc:rightExpr,
        location=top.cnc:exprLocation); }

abstract production regexMatch
e::abs:Expr ::= r::abs:Expr  text::abs:Expr
{

--  int n = matchRegex ( r, text );


  forwards to
    -- matchRegex ( r, t )
    abs:callExpr(
      abs:declRefExpr(
         abs:name("matchRegex", location=e.location),  location=e.location),  

      abs:consExpr( r, 

      abs:consExpr( text,

        abs:nilExpr() ) ) ,

      location=e.location
    ) ;

}




