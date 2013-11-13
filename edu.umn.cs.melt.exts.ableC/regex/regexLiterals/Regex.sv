grammar edu:umn:cs:melt:exts:ableC:regex:regexLiterals ;

imports edu:umn:cs:melt:ableC:concretesyntax as cnc ;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs ;

imports silver:langutil only ast, pp, errors, err ;
imports silver:langutil:pp ;


marking terminal RegexBegin_t '/' ;
terminal RegexEnd_t '/' ;

-- Regex Expressions
concrete production regex_c
e::cnc:PrimaryExpr_c ::= d1::RegexBegin_t  r::Regex_R  d2::RegexEnd_t
layout {}
{
  e.ast = regex("\"" ++ r.regString "\"", location=e.location) ;
}

abstract production regex
e::abs:Expr ::= l1::String
{
  forwards to 
    -- makeRegex ( l1 )
    abs:callExpr(
      abs:declRefExpr(
         abs:name("makeRegex", location=e.location),  location=e.location),  
      abs:consExpr( abs:stringLiteral(l1, location=e.location),
        abs:nilExpr() ) ,
      location=e.location
    ) ;
}


