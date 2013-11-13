grammar edu:umn:cs:melt:ableC:abstractsyntax;

-- In order to accomodate C's odd-ball syntax when it comes to type declarations
-- (with specifiers separate from declarators) we have a divided Type Expressions
-- abstract syntax.

-- BaseTypeExpr represents specifiers: structs, typedefs, ints, etc
-- TypeModifierExpr represents declarators: pointers, arrays, functions, etc.

-- We can't merge these into one TypeExpr because a BaseTypeExpr might be
-- used as part of several declarators.
-- For example, "struct { ... } bar, *baz;"
-- Here, we declare two variables: bar and baz. one of the anonymous struct
-- type, the other a pointer to it. However, we must NOT duplicate the
-- declaration of the struct!
-- That is, we cannot represent it as "struct { ... } bar; struct { ... } *baz;"
-- because that redeclares the type.

-- Our solution is to have a BaseTypeExprs for a declarations, followed by
-- several identifiers each with their own TypeModifiersExpr.
-- This way, the struct appears once in the abstract syntax.

-- TypeModifiersExpr are terminated by "baseTypeExpr" which provides a typerep
-- value that is equal to the Type obtained from the corresponding BaseTypeExpr.

autocopy attribute baseType :: Type;

{-- The TypeExpr is upside down, so build pp from outside-in -}
synthesized attribute lpp :: Document;
synthesized attribute rpp :: Document;

{-- Resolve the TypeExpr into a Type -}
synthesized attribute typerep :: Type;
synthesized attribute typereps :: [Type];

nonterminal TypeName with env, typerep, pp, errors, defs;

abstract production typeName
top::TypeName ::= bty::BaseTypeExpr  mty::TypeModifierExpr
{
  top.pp = concat([bty.pp, mty.lpp, mty.rpp]);
  top.typerep = mty.typerep;
  mty.baseType = bty.typerep;
  top.errors := bty.errors ++ mty.errors;
  top.defs = bty.defs;
}


{--
 - Corresponds to types obtainable from a TypeSpecifiers.
 -}
nonterminal BaseTypeExpr with env, typerep, pp, errors, defs;

function errorTypeExpr
BaseTypeExpr ::= msg::[Message]
{
  return warnTypeExpr(msg, directTypeExpr(errorType()));
}
{-- Raise messages about something syntactic but return ty as the reported type. -}
abstract production warnTypeExpr
top::BaseTypeExpr ::= msg::[Message]  ty::BaseTypeExpr
{
  top.pp = ty.pp;
  top.typerep = ty.typerep;
  top.errors := msg ++ ty.errors;
  top.defs = ty.defs;
}

{-- A TypeExpr that simply yields a type directly, no interpretation necessary.
 - e.g. builtin types. -}
abstract production directTypeExpr
top::BaseTypeExpr ::= result::Type
{
  top.pp = cat(result.lpp, result.rpp);
  top.typerep = result;
  top.errors := [];
  top.defs = [];
}

abstract production tagReferenceTypeExpr
top::BaseTypeExpr ::= q::[Qualifier]  kwd::StructOrEnumOrUnion  name::Name
{
  top.pp = concat([terminate( space(), map( (.pp), q ) ), kwd.pp, space(), name.pp]);

  top.typerep = tagType(q, tagtype); -- TODO: uh, get the thingy somehow here?
  top.errors := [];
  top.defs = [];
  
  -- If the type is indeclared, we report in as an incomplete type, rather than
  -- an undeclared type. So we do *not* do this check:
  --top.errors <- name.tagLookupCheck;

  local tags :: [TagItem] = lookupTag(name.name, top.env);
  local tagtype :: TagType =
    if null(tags) then incompleteTagType(kwd, name.name)
    else error("TODO");
}

nonterminal StructOrEnumOrUnion with pp; -- Silver enums would be nice.
abstract production structSEU
top::StructOrEnumOrUnion ::= { top.pp = text("struct"); }
abstract production unionSEU
top::StructOrEnumOrUnion ::= { top.pp = text("union"); }
abstract production enumSEU
top::StructOrEnumOrUnion ::= { top.pp = text("enum"); }


abstract production structTypeExpr
top::BaseTypeExpr ::= q::[Qualifier]  def::StructDecl
{
  top.pp = concat([ terminate( space(), map( (.pp), q ) ), def.pp ]);
  top.typerep = tagType(q, structTagType(def));
  top.errors := def.errors;
  top.defs = def.defs;
}

abstract production unionTypeExpr
top::BaseTypeExpr ::= q::[Qualifier]  def::UnionDecl
{
  top.pp = concat([ terminate( space(), map( (.pp), q ) ), def.pp ]);
  top.typerep = tagType(q, unionTagType(def));
  top.errors := def.errors;
  top.defs = def.defs;
}

abstract production enumTypeExpr
top::BaseTypeExpr ::= q::[Qualifier]  def::EnumDecl
{
  top.pp = concat([ terminate( space(), map( (.pp), q ) ), def.pp ]);
  top.typerep = tagType(q, enumTagType(def));
  top.errors := def.errors;
  top.defs = def.defs;
}
{-- A name, that needs to be looked up. -}
abstract production typedefTypeExpr
top::BaseTypeExpr ::= q::[Qualifier]  name::Name
{
  top.pp = concat([ terminate( space(), map( (.pp), q ) ), name.pp ]);

  top.typerep = 
    if !null(name.valueLookupCheck) then errorType()
    else noncanonicalType(typedefType(q, name.name, error("todo42"))); -- TODO: broken, need to inject q into that type.
  top.errors := [];
  top.defs = [];

  top.errors <- name.valueLookupCheck; -- TODO: also check that it's typedef.
}



{--
 - Mirrors Type somewhat, but these depend upon the environment.
 - Typically, these are just anchored somewhere to obtain the env,
 - and then turn into an environment-independent Type.
 -}
nonterminal TypeModifierExpr with env, typerep, lpp, rpp, baseType, errors;


abstract production baseTypeExpr
top::TypeModifierExpr ::=
{
  top.lpp = notext();
  top.rpp = notext();
  
  top.typerep = top.baseType; 
  top.errors := [];
}

{-- Pointers -}
abstract production pointerTypeExpr
top::TypeModifierExpr ::= q::[Qualifier]  target::TypeModifierExpr
{
  top.lpp = concat([ target.lpp, space(),
                     text("*"), terminate( space(), map( (.pp), q ) ) ]);
  top.rpp = target.rpp;
  top.typerep = pointerType(q, target.typerep);
  top.errors := target.errors;
}

{-- Arrays (constant, variable, etc) -}
abstract production arrayTypeExprWithExpr
top::TypeModifierExpr ::= element::TypeModifierExpr  indexQualifiers::[Qualifier]  sizeModifier::ArraySizeModifier  size::Expr
{
  top.lpp = element.lpp;
  
  top.rpp = cat(brackets(concat([
    terminate(space(), map((.pp), indexQualifiers)), space(),
    sizeModifier.pp,
    size.pp
    ])), element.rpp);

  top.typerep = arrayType(element.typerep, indexQualifiers, sizeModifier,
    --case size.integerConstantValue of
    --| just(num) -> constantArrayType(num)
    --| nothing() -> variableArrayType(size)
    --end);
    -- TODO: the above needs to be implemented
    error("TODO"));
  top.errors := element.errors ++ size.errors;
}
abstract production arrayTypeExprWithoutExpr
top::TypeModifierExpr ::= element::TypeModifierExpr  indexQualifiers::[Qualifier]  sizeModifier::ArraySizeModifier
{
  top.lpp = element.lpp;
  
  top.rpp = cat(brackets(concat([
    terminate(space(), map((.pp), indexQualifiers)), space(),
    sizeModifier.pp
    ])), element.rpp);

  top.typerep = arrayType(element.typerep, indexQualifiers, sizeModifier, incompleteArrayType());
  top.errors := element.errors;
}

{-- Functions (with or without args) -}
abstract production functionTypeExprWithArgs
top::TypeModifierExpr ::= result::TypeModifierExpr  args::Parameters  variadic::Boolean
{
  top.lpp = concat([ result.lpp ]);
  top.rpp = cat(parens(if null(args.pps) then text("void") else ppImplode(text(", "), args.pps)), result.rpp);
  
  top.typerep = functionType(result.typerep, 
                             protoFunctionType(args.typereps, variadic));
  top.errors := result.errors ++ args.errors;
  
  args.env = openScope(top.env);
}
abstract production functionTypeExprWithoutArgs
top::TypeModifierExpr ::= result::TypeModifierExpr  ids::[Name]  --fnquals::[SpecialSpecifier]
{
  top.lpp = result.lpp;
  top.rpp = cat( parens(ppImplode(text(", "), map((.pp), ids))), result.rpp );
  
  top.typerep = functionType(result.typerep, noProtoFunctionType());
  top.errors := result.errors;
}
{-- Parens -}
abstract production parenTypeExpr
top::TypeModifierExpr ::= wrapped::TypeModifierExpr
{
  --top.pp = parens( wrapped.pp );
  top.lpp = cat( wrapped.lpp, text("(") );
  top.rpp = cat( text(")"), wrapped.rpp );

  top.typerep = noncanonicalType(parenType(wrapped.typerep));
  top.errors := wrapped.errors;
}



{-- Attributes that need to be interpreted away somehow -}
--abstract production attributedTypeExpr
--top::TypeExpr ::= q::[Qualifier]  original::TypeExpr  attr::[Attribute] -- or something?
--{
--  top.typerep = original.typerep; -- {-TODO-};
--}

{-- A name to be looked up in a particular namespace. -}
--abstract production elaboratedTypeExpr
--top::TypeExpr ::= q::[Qualifier]  kwd::StructOrEnumOrUnion  name::String
--{
--  top.typerep = error("todo"); --{-TODO in namespace-};
--}


--nonterminal TypeExprs with env, typereps;

--abstract production consTypeExprs
--top::TypeExprs ::= h::TypeExpr  t::TypeExprs
--{}
--abstract production nilTypeExprs
--top::TypeExprs ::=
--{}

