grammar edu:umn:cs:melt:ableC:abstractsyntax;

{-- Type qualifiers (cv or cvr qualifiers) -}
nonterminal Qualifier with pp;

abstract production constQualifier
top::Qualifier ::=
{
  top.pp = text("const");
}

abstract production volatileQualifier
top::Qualifier ::=
{
  top.pp = text("volatile");
}

abstract production restrictQualifier
top::Qualifier ::=
{
  top.pp = text("restrict");
}

{-- Specifiers that apply to specific types.
 - e.g. Function specifiers (inline, _Noreturn)
 -      Alignment specifiers (_Alignas)
 -}
nonterminal SpecialSpecifier with pp;

abstract production inlineQualifier
top::SpecialSpecifier ::=
{
  top.pp = text("inline");
}


