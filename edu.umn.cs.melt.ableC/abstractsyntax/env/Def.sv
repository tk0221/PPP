grammar edu:umn:cs:melt:ableC:abstractsyntax:env;


abstract production labelDef
top::Def ::= s::String  l::LabelItem
{
  top.labelContribs = [pair(s, l)];
}

abstract production tagDef
top::Def ::= s::String  l::TagItem
{
  top.tagContribs = [pair(s, l)];
}

abstract production valueDef
top::Def ::= s::String  l::ValueItem
{
  top.valueContribs = [pair(s, l)];
}

