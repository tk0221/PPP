
imports silver:langutil;
imports silver:langutil:pp with implode as ppImplode;
imports edu:umn:cs:melt:ableC:abstractsyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;

-- _Noreturn
abstract production noreturnQualifier
top::SpecialSpecifier ::=
{
  top.pp = text("_Noreturn");
}

-- _Generic
abstract production genericSelectionExpr
top::Expr ::= e::Expr  gl::GenericAssocs  def::MaybeExpr
{
  top.pp = concat([text("_Generic"),
    parens(ppImplode(text(", "), e.pp :: gl.pps ++
      if def.isJust then
        [text("default: "), def.pp]
      else
        []
      ))]);
  top.errors := e.errors ++ gl.errors ++ def.errors;
  top.defs = e.defs ++ gl.defs ++ def.defs;
}

nonterminal GenericAssocs with pps, errors, defs, env;

abstract production consGenericAssoc
top::GenericAssocs ::= h::GenericAssoc  t::GenericAssocs
{
  top.pps = h.pp :: t.pps;
  top.errors := h.errors ++ t.errors;
  top.defs = h.defs ++ t.defs;
}
abstract production nilGenericAssoc
top::GenericAssocs ::=
{
  top.pps = [];
  top.errors := [];
  top.defs = [];
}

nonterminal GenericAssoc with location, pp, errors, defs, env;

abstract production genericAssoc
top::GenericAssoc ::= ty::TypeName  fun::Expr
{
  top.pp = concat([ty.pp, text(": "), fun.pp]);
  top.errors := ty.errors ++ fun.errors;
  top.defs = ty.defs ++ fun.defs;
}


-- _Alignas
abstract production alignasSpecifier
top::SpecialSpecifier ::= e::Expr
{
  top.pp = concat([text("_Alignas"), parens(e.pp)]);
--  top.errors := e.errors;
}

-- Alignof
abstract production alignofOp
top::UnaryTypeOp ::=
{
  top.pp = text("_Alignof");
}

-- _Atomic
abstract production atomicTypeExpr
top::BaseTypeExpr ::= q::[Qualifier]  wrapped::TypeName
{
  top.typerep = atomicType(q, wrapped.typerep);
  top.pp = concat([ ppImplode( space(), map( (.pp), q)), space(),
                     text("_Atomic"), parens(wrapped.pp)]);
  top.errors := wrapped.errors;
  top.defs = wrapped.defs;
}

abstract production atomicType
top::Type ::= q::[Qualifier]  bt::Type
{
  top.lpp = concat([ ppImplode( space(), map( (.pp), q)), space(),
                     text("_Atomic"), parens(cat(bt.lpp, bt.rpp))]);
  top.rpp = notext();
}

-- -_Thread_local
abstract production threadLocalStorageClass
top::StorageClass ::=
{
  top.pp = text("_Thread_local");
}


-- _Static_assert
abstract production staticAssertDecl
top::Decl ::= e::Expr  s::String
{
  top.pp = concat([text("_Static_assert("), e.pp, text(", "), text(s), text(");")]);
  top.errors := e.errors;
  top.defs = e.defs;
}


