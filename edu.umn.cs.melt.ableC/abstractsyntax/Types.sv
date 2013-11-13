grammar edu:umn:cs:melt:ableC:abstractsyntax;

{--
 - Type representations, with qualifiers on appropriate constructors,
 - distinct from type expressions as these are environment-independent.
 -
 - Variants: builtin, pointer, array, function, tagged, noncanonical.
 - Noncanonical forwards, and so doesn't need any attributes, etc attached to it.
 -}
nonterminal Type with lpp, rpp;

{-------------------------------------------------------------------------------
 - An error occurred while trying to construct the type.
 - Typically, this should be considered "compatible" with any type: an error
 - has already been raised, further errors are redundant.
 -}
abstract production errorType
top::Type ::=
{
  top.lpp = text("/*err*/");
  top.rpp = text("");
}


{-------------------------------------------------------------------------------
 - Builtin C types: void, unsigned int, signed char, float, bool, etc.
 - also includes complex numbers e.g. 'float _Complex'
 -}
abstract production builtinType
top::Type ::= q::[Qualifier]  bt::BuiltinType
{
  top.lpp =
    concat([terminate(space(), map((.pp), q)),
            bt.pp]);
  top.rpp = notext();
}


{-------------------------------------------------------------------------------
 - Pointer to another type.
 - e.g.  'const int * foo' is pointer([], builtin([const], int)).
 - while 'int * const foo' is pointer([const], builtin([], int)).
 -}
abstract production pointerType
top::Type ::= q::[Qualifier]  target::Type
{
  top.lpp = concat([ target.lpp, space(), ppImplode( space(), map( (.pp), q ) ),
                     text("*") ]);
  top.rpp = target.rpp;
}


{-------------------------------------------------------------------------------
 - Arrays
 -
 - @param element  The array element type. e.g. int for 'array of int'
 - @param sub  Constant, variable, or incomplete array size?
 -
 - The following are *only* valid in function parameter types (at the 'top' of the type):
 -
 - @param indexQualifiers  Qualifiers to attach to a decayed pointer type
 -          e.g. 'char foo[restrict 10]' decays to a 'restrict char *'
 - @param sizeModifier  None, typically, but may be 'static' or 'star'
 -          star is further valid only in function *prototypes* not even definitions.
 -          star means "is a complete array type, but I'm not saying how here"
 -          static means "must be an array with this many elements"
 -
 - It'd be nice to make these "noncanonical" somehow, but I'm not sure how.
 - They actually do matter for the purposes of checking 'compatible types' for
 - example, which kinda makes them canonical, even if they never appear outside
 - function parameters.
 -}
abstract production arrayType
top::Type ::= element::Type  indexQualifiers::[Qualifier]  sizeModifier::ArraySizeModifier  sub::ArrayType
{
  top.lpp = element.lpp;
  
  top.rpp = cat(brackets(concat([
    terminate(space(), map((.pp), indexQualifiers)),
    sizeModifier.pp,
    sub.pp
    ])), element.rpp);
}

{-- The subtypes of arrays -}
nonterminal ArrayType with pp;

abstract production constantArrayType
top::ArrayType ::= size::Integer
{
  top.pp = text(toString(size));
  -- TODO: include the Decorated Expr here too maybe?
}
abstract production incompleteArrayType
top::ArrayType ::=
{
  top.pp = notext();
}
abstract production variableArrayType
top::ArrayType ::= size::Decorated Expr
{
  top.pp = size.pp;
}

{-- Modifiers attached to array types that are function parameters -}
nonterminal ArraySizeModifier with pp;
{-- Normal array. -}
abstract production normalArraySize
top::ArraySizeModifier ::= { top.pp = notext(); }
{-- Array must have at least this many arguments -}
abstract production staticArraySize
top::ArraySizeModifier ::= { top.pp = text("static"); }
{-- Array of unknown but fixed size. Valid only in prototypes, not definitions. -}
abstract production starArraySize
top::ArraySizeModifier ::= { top.pp = text("*"); }


{-------------------------------------------------------------------------------
 - Functions (with or without args)
 - Evidentally, old-style K&R function *types* do not contain any information
 - about parameter types. Not even number.
 -}
abstract production functionType
top::Type ::= result::Type  sub::FunctionType
{
  --TODO ???
  top.lpp = concat([ sub.lpp, space(), result.lpp ]);
  top.rpp = cat(sub.rpp, result.rpp);
}

{-- The subtypes of functions -}
nonterminal FunctionType with lpp, rpp;
-- clang has an 'extinfo' structure with calling convention, noreturn, 'produces'?, regparam

abstract production protoFunctionType
top::FunctionType ::= args::[Type]  variadic::Boolean
{
  --TODO ???
  top.lpp = notext(); --terminate( space(), map( (.pp), qualifiers ) );
  top.rpp = parens(
    if null(args) then
      text("void")
    else
      ppImplode(text(", "), argpp)
    );
  local argpp :: [Document] =
    zipWith(cat,
      map((.lpp), args),
      map((.rpp), args)) ++ if variadic then [] else [text("...")];
}
-- Evidently, old K&R C functions don't have args as part of function type
abstract production noProtoFunctionType
top::FunctionType ::=
{
  --TODO ???
  top.lpp = notext();
  top.rpp = text("()");
}


{-------------------------------------------------------------------------------
 - Tagged types: enum, struct, union
 -}
abstract production tagType
top::Type ::= q::[Qualifier]  sub::TagType
{
  top.lpp = concat([ terminate( space(), map( (.pp), q ) ), sub.pp ]);
  top.rpp = notext();
}

{-- Structs, unions and enums -}
nonterminal TagType with pp;

abstract production structTagType
top::TagType ::= ref::Decorated StructDecl
{
  top.pp =
    case ref.maybename of
    | just(n) -> cat(text("struct "), n.pp)
    | nothing() -> text("<anon struct>") -- TODO: location
    end;
}
abstract production unionTagType
top::TagType ::= ref::Decorated UnionDecl
{
  top.pp =
    case ref.maybename of
    | just(n) -> cat(text("union "), n.pp)
    | nothing() -> text("<anon struct>") -- TODO: location
    end;
}
abstract production enumTagType
top::TagType ::= ref::Decorated EnumDecl
{
  top.pp =
    case ref.maybename of
    | just(n) -> cat(text("enum "), n.pp)
    | nothing() -> text("<anon struct>") -- TODO: location
    end;
}
abstract production incompleteTagType
top::TagType ::= kwd::StructOrEnumOrUnion  name::String
{
  top.pp = concat([kwd.pp, text(name)]);
}

{-------------------------------------------------------------------------------
 - Types that resolve to other types
 -}
abstract production noncanonicalType
top::Type ::= sub::NoncanonicalType
{
  top.lpp = sub.lpp;
  top.rpp = sub.rpp;
  
  forwards to sub.canonicalType;
}

{-- Types that resolve to other types. -}
nonterminal NoncanonicalType with canonicalType, lpp, rpp;

synthesized attribute canonicalType :: Type;

{-- Parens. 
 - No qualifiers, as it's not even syntactically possible. This exists for no
 - reason but to mirror the type exactly as the programmer wrote it.
 - e.g. 'const ((footypedef))' if that's their preferred thing.
 -
 - Note: parens are weird in that they're "inside out". That is, the thing that's
 - in parenthesis is what's above us in the type tree.
 - e.g. 'pointer(paren(int))' is a pointer to an int, but it's written 'int (*)'
 - with the parens around the 'pointer to' not the 'int'
 -
 - There's a good (almost sorta attribute grammary) pretty printer example of
 - how to render types in the presence of this sort of thing in Clang.
 -}
abstract production parenType
top::NoncanonicalType ::= wrapped::Type
{
  top.lpp = concat([ wrapped.lpp, space(), text("(") ]);
  top.rpp = cat( text(")"), wrapped.rpp );

  top.canonicalType = wrapped;
}

{-- Represents "decayed pointers" from array or function type.
 -
 - The "default conversions" should turn array types into
 - pointer types, but we want to be able to report the orginal
 - array type in error messages, for example.
 -
 - e.g. 'char foo[2];' when we write 'foo[1]' and consult
 - the type of the 'foo' subexpression, we would then perform
 - default conversions, getting 'char *' which isn't the ideal
 - type to report if there's an error. 
 -
 - This exists to provide us a way to preserve
 - the original type, while still decaying in the conversion.
 - 'char [2]' can be converted instead to 'decayed(char [2], char *)'
 - which then forwards to char *.
 -}
abstract production decayedType
top::NoncanonicalType ::= original::Type  pointer::Type
{
  top.lpp = original.lpp;
  top.rpp = original.rpp;

  top.canonicalType = pointer;
}

{-- A name, that has already been looked up, but of course we'd like to
 - refer to it by name if possible.
 -
 - Invariant: any qualifiers on this typedef (i.e. 'q')
 - have already been injected into 'resolved'.
 -
 - They are only preserved here for essentially pp purposes. 
 - e.g. given: typedef volatile struct foo { } Foo;
 -    'const Foo' will have 'const' in q, and 'resolved' will have const and volatile. -}
abstract production typedefType
top::NoncanonicalType ::= q::[Qualifier]  name::String  resolved::Type
{
  top.lpp = concat([ terminate( space(), map( (.pp), q ) ), text(name) ]);
  top.rpp = notext();  

  top.canonicalType = resolved;
}


{-- Attributes that need to be interpreted away somehow. What is this?
 - Evidently, part of extensions... maybe we should remove this.
 - e.g. int32 __attribute((vector(4)))__ is supposed to be a SSE register or something
 -}
--abstract production attributedType
--top::NoncanonicalType ::= q::[Qualifier]  original::Type  attr::[Attribute] -- or something?
--{
--  top.canonicalType = error("?"); -- TODO: Perhaps this is supposed to have already been interepreted somehow
--}

{-- Explicit representation of 'struct foo' 
 - instead of immediately recognizing this as a tagType?
 - TODO: not sure why we need this, really.
 - Maybe we should change this.
 -}
--abstract production elaboratedType
--top::NoncanonicalType ::= q::[Qualifier]  kwd::StructOrEnumOrUnion  name::String  resolved::Type
--{
--  top.canonicalType = resolved; -- I guess assume resolved has qualifiers already injected into it.
--}









{- TODO from clang:
NON_CANONICAL_UNLESS_DEPENDENT_TYPE(TypeOfExpr, Type)
NON_CANONICAL_UNLESS_DEPENDENT_TYPE(TypeOf, Type)

-- Evidently, GCC extensions for SIMD vector types
 TYPE(Vector, Type)
 TYPE(ExtVector, VectorType)
-}

