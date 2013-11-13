grammar edu:umn:cs:melt:ableC:abstractsyntax:env;

type Scope<a> = [tm:Map<String a>];
type Contribs<a> = [Pair<String a>];

synthesized attribute labels :: Scope<LabelItem>;
synthesized attribute tags :: Scope<TagItem>;
synthesized attribute values :: Scope<ValueItem>;

synthesized attribute labelContribs :: Contribs<LabelItem>;
synthesized attribute tagContribs :: Contribs<TagItem>;
synthesized attribute valueContribs :: Contribs<ValueItem>;

function augmentScope_i
Scope<a> ::= d::Contribs<a>  s::Scope<a>
{
  return tm:add(d, head(s)) :: tail(s);
}
function readScope_i
[a] ::= name::String  scope::Scope<a>
{
  -- Laziness is awesome.
  return case dropWhile(null, map(tm:lookup(name, _), scope)) of
  | h :: _ -> h
  | [] -> []
  end;
}

-- Environment representation productions

abstract production emptyEnv_i
top::Env ::=
{
  top.labels = [tm:empty(compareString)];
  top.tags = [tm:empty(compareString)];
  top.values = [tm:empty(compareString)];
}
abstract production addEnv_i
top::Env ::= d::Defs  e::Decorated Env
{
  top.labels = augmentScope_i(d.labelContribs, e.labels);
  top.tags = augmentScope_i(d.tagContribs, e.tags);
  top.values = augmentScope_i(d.valueContribs, e.values);
}
abstract production openScope_i
top::Env ::= e::Decorated Env
{
  top.labels = tm:empty(compareString) :: e.labels;
  top.tags = tm:empty(compareString) :: e.tags;
  top.values = tm:empty(compareString) :: e.values;
}

-- Definition list productions

abstract production nilDefs
top::Defs ::=
{
  top.labelContribs = [];
  top.tagContribs = [];
  top.valueContribs = [];
}

abstract production consDefs
top::Defs ::= h::Def  t::Defs
{
  top.labelContribs = h.labelContribs ++ t.labelContribs;
  top.tagContribs = h.tagContribs ++ t.tagContribs;
  top.valueContribs = h.valueContribs ++ t.valueContribs;
}

-- Defaults for Def

aspect default production
top::Def ::=
{
  top.labelContribs = [];
  top.tagContribs = [];
  top.valueContribs = [];
}

