
autocopy attribute op :: Decorated Expr;
synthesized attribute preExpr :: Boolean;

nonterminal UnaryOp with location, op, pp, preExpr;

abstract production preIncOp
top::UnaryOp ::=
{
  top.pp = text("++");
  top.preExpr = true;
}
abstract production preDecOp
top::UnaryOp ::= 
{
  top.pp = text("--");
  top.preExpr = true;
}
abstract production postIncOp
top::UnaryOp ::= 
{
  top.pp = text("++");
  top.preExpr = false;
}
abstract production postDecOp
top::UnaryOp ::= 
{
  top.pp = text("--");
  top.preExpr = false;
}
abstract production addressOfOp
top::UnaryOp ::=
{
  top.pp = text("&");
  top.preExpr = true;
}
abstract production dereferenceOp
top::UnaryOp ::=
{
  top.pp = text("*");
  top.preExpr = true;
}
abstract production positiveOp
top::UnaryOp ::=
{
  top.pp = text("+");
  top.preExpr = true;
}
abstract production negativeOp
top::UnaryOp ::=
{
  top.pp = text("-");
  top.preExpr = true;
}
abstract production bitNegateOp
top::UnaryOp ::=
{
  top.pp = text("~");
  top.preExpr = true;
}
abstract production notOp
top::UnaryOp ::=
{
  top.pp = text("!");
  top.preExpr = true;
}

abstract production warnNoOp
top::UnaryOp ::= msg::[Message]
{
  top.pp = text("");
  top.preExpr = true;
}



autocopy attribute typeop :: Type;

nonterminal UnaryTypeOp with location, typeop, pp;

abstract production sizeofOp
top::UnaryTypeOp ::=
{
  top.pp = text("sizeof");
}

