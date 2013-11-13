

nonterminal MaybeExpr with pp, isJust, errors, defs, env;

abstract production justExpr
top::MaybeExpr ::= e::Expr
{
  top.pp = e.pp;
  top.isJust = true;
  top.errors := e.errors;
  top.defs = e.defs;
}
abstract production nothingExpr
top::MaybeExpr ::=
{
  top.pp = notext();
  top.isJust = false;
  top.errors := [];
  top.defs = [];
}


synthesized attribute pps ::[Document];

nonterminal Exprs with pps, errors, defs, env;


abstract production consExpr
top::Exprs ::= h::Expr  t::Exprs
{
  top.pps = h.pp :: t.pps;
  top.errors := h.errors ++ t.errors;
  top.defs = h.defs ++ t.defs;
  
  t.env = addEnv(h.defs, h.env);
}
abstract production nilExpr
top::Exprs ::=
{
  top.pps = [];
  top.errors := [];
  top.defs = [];
}

nonterminal ExprOrTypeName with pp, errors, defs, env;

abstract production exprExpr
top::ExprOrTypeName ::= e::Expr
{
  top.pp = e.pp;
  top.errors := e.errors;
  top.defs = e.defs;
}
abstract production typeNameExpr
top::ExprOrTypeName ::= ty::TypeName
{
  top.pp = ty.pp;
  top.errors := ty.errors;
  top.defs = ty.defs;
}


