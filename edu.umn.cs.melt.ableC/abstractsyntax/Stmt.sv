
nonterminal Stmt with pp, errors, defs, env, functiondefs;

abstract production nullStmt
top::Stmt ::=
{
  top.pp = notext();
  top.errors := [];
  top.defs = [];
  top.functiondefs = [];
}

abstract production compoundStmt
top::Stmt ::= h::Stmt  t::Stmt
{
  top.pp = concat([ h.pp, line(), t.pp ]);
  top.errors := h.errors ++ t.errors;
  top.defs = h.defs ++ t.defs;
  top.functiondefs = h.functiondefs ++ t.functiondefs;
  
  t.env = addEnv(h.defs, top.env);
}

-- ditto warnExternalDecl, if warning or empty, then this pretends it doesn't exist.
abstract production warnStmt
top::Stmt ::= msg::[Message]
{
  top.pp = text("/*err*/");
  top.errors := msg;
  top.defs = [];
  top.functiondefs = [];
}

abstract production declStmt
top::Stmt ::= d::Decl
{
  top.pp = cat( d.pp, semi() );
  top.errors := d.errors;
  top.defs = d.defs;
  top.functiondefs = [];
}

abstract production exprStmt
top::Stmt ::= d::Expr
{
  top.pp = cat( d.pp, semi() );
  top.errors := d.errors;
  top.defs = d.defs;
  top.functiondefs = [];
}

abstract production ifStmt
top::Stmt ::= c::Expr  t::Stmt  e::Stmt
{
  top.pp = concat([
    text("if"), space(), parens(c.pp), line(),
    braces(nestlines(2, t.pp)) ] ++
      case e of nullStmt() -> []
      | _ -> [
          text(" else "),
          braces(nestlines(2, e.pp))]
      end);
  top.errors := c.errors ++ t.errors ++ e.errors;
  top.defs = [];
  top.functiondefs = t.functiondefs ++ e.functiondefs;
  
  local newEnv :: Decorated Env = addEnv(c.defs, openScope(top.env));
  t.env = newEnv;
  e.env = newEnv;
}

abstract production whileStmt
top::Stmt ::= e::Expr  b::Stmt
{
  top.pp = concat([ text("while"), space(), parens(e.pp), line(), 
                    braces(nestlines(2, b.pp)) ]);
  top.errors := e.errors ++ b.errors;
  top.defs = [];
  top.functiondefs = b.functiondefs;
  
  b.env = addEnv(e.defs, openScope(top.env));
}

abstract production doStmt
top::Stmt ::= b::Stmt  e::Expr
{
  top.pp = concat([ text("do"),  line(), 
                    braces(nestlines(2,b.pp)), line(), 
                    text("while"), space(), parens(e.pp), semi()]);
  top.errors := b.errors ++ e.errors;
  top.defs = [];
  top.functiondefs = b.functiondefs;
  
  b.env = openScope(top.env);
  e.env = addEnv(b.defs, b.env);
}

abstract production forStmt
top::Stmt ::= i::MaybeExpr  c::MaybeExpr  s::MaybeExpr  b::Stmt
{
  top.pp = concat([ text("for"), space(), parens( concat([i.pp, semi(), space(), c.pp, semi(), space(), s.pp]) ), line(), 
                    braces(nestlines(2, b.pp)) ]);
  top.errors := i.errors ++ c.errors ++ s.errors ++ b.errors;
  
  top.defs = [];
  top.functiondefs = b.functiondefs;
  
  i.env = openScope(top.env);
  c.env = addEnv(i.defs, i.env);
  s.env = addEnv(c.defs, c.env);
  b.env = addEnv(s.defs, s.env);
}

abstract production forDeclStmt
top::Stmt ::= i::Decls  c::MaybeExpr  s::MaybeExpr  b::Stmt
{
  top.pp = concat([ text("for"), space(), parens( concat([ ppImplode( cat( comma(), space() ), i.pps ), 
                                                           semi(), space(), c.pp, semi(), space(), s.pp]) ), 
                    line(), braces(nestlines(2, b.pp)) ]);
  top.errors := i.errors ++ c.errors ++ s.errors ++ b.errors;
  top.defs = [];
  top.functiondefs = b.functiondefs;
  
  i.env = openScope(top.env);
  c.env = addEnv(i.defs, i.env);
  s.env = addEnv(c.defs, c.env);
  b.env = addEnv(s.defs, s.env);
}

abstract production returnStmt
top::Stmt ::= e::MaybeExpr
{
  top.pp = concat([text("return"), space(), e.pp, semi()]);
  top.errors := e.errors;
  top.defs = e.defs;
  top.functiondefs = [];
}

abstract production switchStmt
top::Stmt ::= e::Expr  b::Stmt
{
  top.pp = concat([ text("switch"), space(), parens(e.pp),  line(), 
                    braces(nestlines(2, b.pp)) ]);
  top.errors := e.errors ++ b.errors;
  top.defs = [];
  
  e.env = openScope(top.env);
  b.env = addEnv(e.defs, e.env);
  top.functiondefs = b.functiondefs;
}

abstract production gotoStmt
top::Stmt ::= l::Name
{
  top.pp = concat([ text("goto"), space(), l.pp, semi() ]);
  top.errors := [];
  top.defs = [];
  top.functiondefs = [];
  
  top.errors <- l.labelLookupCheck;
}

abstract production continueStmt
top::Stmt ::=
{
  top.pp = cat( text("continue"), semi() );
  top.errors := [];
  top.defs = [];
  top.functiondefs = [];
}

abstract production breakStmt
top::Stmt ::=
{
  top.pp = concat([ text("break"), semi()  ]);
  top.errors := [];
  top.defs = [];
  top.functiondefs = [];
}

abstract production labelStmt
top::Stmt ::= l::Name  s::Stmt
{
  top.pp = concat([ l.pp, text(":"), space(), s.pp]);
  top.errors := s.errors;
  top.defs = s.defs;
  top.functiondefs = [labelDef(l.name, labelItem(top))] ++ s.functiondefs;
  
  top.errors <- l.labelRedeclarationCheck;
}

abstract production caseLabelStmt
top::Stmt ::= v::Expr  s::Stmt
{
  top.pp = concat([text("case"), space(), v.pp, text(":"), space(), s.pp]); 
  top.errors := v.errors ++ s.errors;
  top.defs = v.defs ++ s.defs;
  top.functiondefs = s.functiondefs; -- ??
  
  s.env = addEnv(v.defs, v.env);
}

abstract production defaultLabelStmt
top::Stmt ::= s::Stmt
{
  top.pp = concat([ text("default"), text(":"), space(), s.pp]);
  top.errors := s.errors;
  top.defs = s.defs;
  top.functiondefs = s.functiondefs; -- ??
}



{- from clang:

abstract production
top::Stmt ::=
{
}

def NullStmt : Stmt;
def CompoundStmt : Stmt;
def LabelStmt : Stmt;
def IfStmt : Stmt;
def SwitchStmt : Stmt;
def WhileStmt : Stmt;
def DoStmt : Stmt;
def ForStmt : Stmt;
def GotoStmt : Stmt;
def IndirectGotoStmt : Stmt;   -- TODO?
def ContinueStmt : Stmt;
def BreakStmt : Stmt;
def ReturnStmt : Stmt;
def DeclStmt  : Stmt;
def SwitchCase : Stmt<1>;
def CaseStmt : DStmt<SwitchCase>;
def DefaultStmt : DStmt<SwitchCase>;


def AttributedStmt : Stmt;  -- no worries yet, gcc ext
def CapturedStmt : Stmt;  -- no worries yet, this is something different (e.g. omp parts)

// Asm statements
def AsmStmt : Stmt<1>;
def GCCAsmStmt : DStmt<AsmStmt>;
def MSAsmStmt : DStmt<AsmStmt>;

-}

