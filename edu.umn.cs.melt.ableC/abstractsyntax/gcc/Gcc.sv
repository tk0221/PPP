
imports edu:umn:cs:melt:ableC:abstractsyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports silver:langutil;
imports silver:langutil:pp;


abstract production vaListTypeExpr
top::BaseTypeExpr ::=
{
  top.typerep = pointerType([], builtinType([], voidType())); -- TODO
  top.pp = text("__builtin_va_list");
  top.errors := [];
  top.defs = [];
}
abstract production typeofTypeExpr
top::BaseTypeExpr ::= q::[Qualifier]  e::ExprOrTypeName
{
  top.typerep = noncanonicalType(typeofType(q, error("e.typerep"))); -- TODO
  top.pp = concat([text("__typeof__"), parens(e.pp)]);
  top.errors := e.errors;
  top.defs = e.defs;
}

abstract production typeofType
top::NoncanonicalType ::= q::[Qualifier]  resolved::Type
{
  top.canonicalType = resolved;-- todo: some sort of discipline of what to do with qualifiers here
  top.lpp = concat([text("__typeof__"), parens(cat(resolved.lpp, resolved.rpp))]);
  top.rpp = notext();
}

abstract production typesCompatibleExpr
top::Expr ::= l::TypeName  r::TypeName
{
  top.pp = concat([text("__builtin_types_compatible_p("), l.pp, text(", "), r.pp, text(")")]);
  top.errors := l.errors ++ r.errors;
  top.defs = l.defs ++ r.defs;
}
abstract production vaArgExpr
top::Expr ::= e::Expr  ty::TypeName
{
  top.pp = concat([text("__builtin_va_arg("), e.pp, text(", "), ty.pp, text(")")]);
  top.errors := e.errors ++ ty.errors;
  top.defs = e.defs ++ ty.defs;
}
abstract production stmtExpr
top::Expr ::= s::Stmt -- TODO figure out how to get "return value"?
{
  top.pp = concat([text("({"), s.pp, text("})")]);
  top.errors := s.errors;
  top.defs = []; -- TODO: ?
}

abstract production arrayRangeDesignator
top::Designator ::= d::Designator  l::Expr  u::Expr
{
  top.pp = concat([d.pp, text("["), l.pp, text("..."), u.pp, text("]")]);
  top.errors := d.errors ++ l.errors ++ u.errors;
  top.defs = d.defs ++ l.defs ++ u.defs;
}

abstract production offsetofExpr
top::Expr ::= ty::TypeName  e::MemberDesignator
{
  top.pp = concat([text("__builtin_offsetof("), ty.pp, text(", "), e.pp, text(")")]);
  top.errors := ty.errors ++ e.errors;
  top.defs = ty.defs ++ e.defs;
}

nonterminal MemberDesignator with pp, errors, defs, env;

abstract production initialMemberDesignator
top::MemberDesignator ::= id::Name
{
  top.pp = id.pp;
  top.errors := [];
  top.defs = [];
}
abstract production fieldMemberDesignator
top::MemberDesignator ::= d::MemberDesignator  id::Name
{
  top.pp = concat([d.pp, text("."), id.pp]);
  top.errors := d.errors;
  top.defs = d.defs;
}
abstract production derefMemberDesignator
top::MemberDesignator ::= d::MemberDesignator  id::Name
{
  top.pp = concat([d.pp, text("->"), id.pp]);
  top.errors := d.errors;
  top.defs = d.defs;
}
abstract production arrayMemberDesignator
top::MemberDesignator ::= d::MemberDesignator  e::Expr
{
  top.pp = concat([d.pp, text("["), e.pp, text("]")]);
  top.errors := d.errors;
  top.defs = d.defs ++ e.defs; -- sigh
}

abstract production imaginaryLiteral
top::Expr ::= l::String
{
  top.pp = text(l);
  top.errors := [];
  top.defs = [];
}
abstract production imaginaryHexLiteral
top::Expr ::= l::String
{
  top.pp = text(l);
  top.errors := [];
  top.defs = [];
}

abstract production realOp
top::UnaryOp ::=
{
  top.pp = text("__real__");
  top.preExpr = true;
}
abstract production imagOp
top::UnaryOp ::=
{
  top.pp = text("__imag__");
  top.preExpr = true;
}

-- TODO: we need to be very consistent about how we deal with these, then.
-- Currently, they're ignored by too many declaration thingys. they shouldn't be.
-- i.e. the concrete syntax has no place to put them on the abstract.
abstract production attributeQualifier
top::SpecialSpecifier ::=
{
  top.pp = text("__attribute__ TODO");
}

abstract production functionDeclStmt
top::Stmt ::= d::FunctionDecl
{
  top.pp = d.pp;
  top.errors := d.errors;
  top.defs = d.defs;
  top.functiondefs = [];
}

abstract production caseLabelRangeStmt
top::Stmt ::= l::Expr  u::Expr  s::Stmt
{
  top.pp = concat([text("case"), space(), l.pp, text("..."), u.pp, text(":"), space(),s.pp]); 
  top.errors := l.errors ++ u.errors ++ s.errors;
  top.defs = l.defs ++ u.defs ++ s.defs;
  top.functiondefs = s.functiondefs;
}

abstract production binaryConditionalExpr
top::Expr ::= cond::Expr  e::Expr
{
  top.pp = concat([ cond.pp, space(), text("?:"), space(), e.pp]);
  top.errors := cond.errors ++ e.errors;
  top.defs = cond.defs ++ e.defs;
}

