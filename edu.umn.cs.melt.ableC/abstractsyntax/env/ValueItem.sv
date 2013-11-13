grammar edu:umn:cs:melt:ableC:abstractsyntax:env;

nonterminal ValueItem;

-- TODO: we might consider splitting this into values and typedef names.
-- but, perhaps that is unnecessary. defer to later.
abstract production declaratorValueItem
top::ValueItem ::= s::Decorated Declarator
{
}

abstract production functionValueItem
top::ValueItem ::= s::Decorated FunctionDecl
{
}

abstract production fieldValueItem
top::ValueItem ::= s::Decorated StructDeclarator
{
}

abstract production enumValueItem
top::ValueItem ::= s::Decorated EnumItem
{
}

abstract production parameterValueItem
top::ValueItem ::= s::Decorated ParameterDecl
{
}

abstract production errorValueItem
top::ValueItem ::=
{
}

