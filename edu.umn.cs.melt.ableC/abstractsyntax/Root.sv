

nonterminal Root with pp, errors;

abstract production root
top::Root ::= d::ExternalDecls
{
  top.pp = d.pp;
  top.errors := d.errors;
  d.env = emptyEnv();
}



nonterminal ExternalDecls with pp, errors, env;

abstract production consExternalDecl
top::ExternalDecls ::= h::ExternalDecl  t::ExternalDecls
{
  top.pp = concat([h.pp, line(), t.pp]);
  top.errors := h.errors ++ t.errors;
  
  t.env = addEnv(h.defs, top.env);
}

abstract production nilExternalDecl
top::ExternalDecls ::=
{
  top.pp = notext();
  top.errors := [];
}


nonterminal ExternalDecl with pp, errors, defs, env;

abstract production declExternalDecl
top::ExternalDecl ::= d::Decl
{
  top.pp = cat(d.pp, semi());
  top.errors := d.errors;
  top.defs = d.defs;
}

abstract production functionExternalDecl
top::ExternalDecl ::= f::FunctionDecl
{
  top.pp = f.pp;
  top.errors := f.errors;
  top.defs = f.defs;
}

{--
 - The semantics of this are to raise the messages in 'msg' and otherwise have
 - no effect. This is distinct from typical 'errorSomething' productions
 - in that the messages can be warnings about something, but this production
 - will otherwise behave as though it didn't exist. (whereas an 'error' production
 - typically has no interpretation beyond raising errors.)
 -}
abstract production warnExternalDecl
top::ExternalDecl ::= msg::[Message]
{
  top.pp = text("/*err*/");
  top.errors := msg;
  top.defs = [];
}



