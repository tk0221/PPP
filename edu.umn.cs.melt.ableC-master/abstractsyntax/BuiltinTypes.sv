grammar edu:umn:cs:melt:ableC:abstractsyntax;

{--
 - Primitive C types.
 - Design note: signed and unsigned having 'IntegerType' is ableC's own idiosyntactic design.
 - This can be changed if it turns out that's an annoying choice somehow.
 -}
nonterminal BuiltinType with pp;

-- It might be nice to have an enum in Silver or something to represent these.
-- That's Clang's design, complete with duplicate entries for signed/unsigned ints.

{-- void -}
abstract production voidType
top::BuiltinType ::=
{
  top.pp = text("void");
}

{-- _Bool -}
abstract production boolType
top::BuiltinType ::=
{
  top.pp = text("_Bool");
}

{-- any real type -}
abstract production realType
top::BuiltinType ::= rt::RealType
{
  top.pp = rt.pp;
}

{-- any _Complex type -}
abstract production complexType
top::BuiltinType ::= rt::RealType
{
  top.pp = concat([ text("_Complex"), space(), rt.pp ]);
}

{-- any _Imaginary type -}
abstract production imaginaryType
top::BuiltinType ::= rt::RealType
{
  top.pp = concat([ text("_Imaginary"), space(), rt.pp ]);
}

{-- any signed integer type -}
abstract production signedType
top::BuiltinType ::= it::IntegerType
{
  top.pp = concat([ text("signed"), space(), it.pp ]);
}

{-- any unsigned integer type -}
abstract production unsignedType
top::BuiltinType ::= it::IntegerType
{
  top.pp = concat([ text("unsigned"), space(), it.pp ]);
}

{-- any _Complex *integer* type -- probably a gcc extension? -}
abstract production complexIntegerType
top::BuiltinType ::= rt::IntegerType
{
  top.pp = concat([ text("_Complex"), space(), rt.pp ]);
}


{-- Floating types, for which there is a normal and complex variant -}
nonterminal RealType with pp;

abstract production floatType
top::RealType ::=
{
  top.pp = text("float");
}

abstract production doubleType
top::RealType ::=
{
  top.pp = text("double");
}

abstract production longdoubleType
top::RealType ::=
{
  top.pp = text("long double");
}


{-- Integer types, for which there is a signed and unsigned variant -}
nonterminal IntegerType with pp;

abstract production charType
top::IntegerType ::=
{
  top.pp = text("char");
}

abstract production shortType
top::IntegerType ::=
{
  top.pp = text("short");
}

abstract production intType
top::IntegerType ::=
{
  top.pp = text("int");
}

abstract production longType
top::IntegerType ::=
{
  top.pp = text("long");
}

abstract production longlongType
top::IntegerType ::=
{
  top.pp = text("long long");
}

abstract production int128Type
top::IntegerType ::=
{
  top.pp = text("__int128");
}

