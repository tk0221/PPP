grammar edu:umn:cs:melt:ableC:abstractsyntax:env;

nonterminal TagItem;


abstract production structTagItem
top::TagItem ::= s::Decorated StructDecl
{
}

abstract production unionTagItem
top::TagItem ::= s::Decorated UnionDecl
{
}

abstract production enumTagItem
top::TagItem ::= s::Decorated EnumDecl
{
}

abstract production errorTagItem
top::TagItem ::=
{
}

