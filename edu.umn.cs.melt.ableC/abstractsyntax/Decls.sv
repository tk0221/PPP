
-- StructDecl, UnionDecl, and EnumDecl are all rooted in the abstract syntax within a TypeExpr.

-- FunctionDecl is (for now) always rooted in an ExternalDeclaration
-- Declaration is rooted in External, but also in stmts. Either a variableDecl or a typedefDecl.
-- ParameterDecl should probably be something special, distinct from variableDecl.

nonterminal Decls with pps, errors, defs, env;

abstract production consDecl
top::Decls ::= h::Decl  t::Decls
{
  top.pps = h.pp :: t.pps;
  top.errors := h.errors ++ t.errors;
  top.defs = h.defs ++ t.defs;
  
  t.env = addEnv(h.defs, top.env);
}

abstract production nilDecl
top::Decls ::=
{
  top.pps = [];
  top.errors := [];
  top.defs = [];
}

nonterminal Decl with pp, errors, defs, env;

abstract production variableDecls
top::Decl ::= storage::[StorageClass]  ty::BaseTypeExpr  dcls::Declarators
{
  top.pp = concat(
    terminate(space(), map((.pp), storage)) ::
      [ty.pp, space(), ppImplode(text(", "), dcls.pps)]);
  top.errors := ty.errors ++ dcls.errors;
  top.defs = ty.defs ++ dcls.defs;
  
  dcls.baseType = ty.typerep;
}

abstract production typeExprDecl
top::Decl ::= ty::BaseTypeExpr
{
  top.pp = ty.pp;
  top.errors := ty.errors;
  top.defs = ty.defs;
}

abstract production typedefDecls
top::Decl ::= ty::BaseTypeExpr  dcls::Declarators
{
  top.pp = concat([text("typedef"), space(), ty.pp, space(), ppImplode(text(", "), dcls.pps)]);
  top.errors := ty.errors ++ dcls.errors;
  top.defs = ty.defs ++ dcls.defs;
  
  dcls.baseType = ty.typerep;
}

nonterminal Declarators with pps, errors, defs, env, baseType;

abstract production consDeclarator
top::Declarators ::= h::Declarator  t::Declarators
{
  top.pps = h.pps ++ t.pps;
  top.errors := h.errors ++ t.errors;
  top.defs = h.defs ++ t.defs;
}
abstract production nilDeclarator
top::Declarators ::=
{
  top.pps = [];
  top.errors := [];
  top.defs = [];
}

nonterminal Declarator with pps, errors, defs, env, baseType, typerep;

abstract production declarator
top::Declarator ::= name::Name  ty::TypeModifierExpr  initializer::MaybeInitializer
{
  top.pps = [concat([ty.lpp, name.pp, ty.rpp, initializer.pp])];
  top.errors := ty.errors ++ initializer.errors;
  top.defs = [valueDef(name.name, declaratorValueItem(top))];
  top.typerep = ty.typerep;
  
  top.errors <- name.valueRedeclarationCheck;
}
abstract production errorDeclarator
top::Declarator ::= msg::[Message]
{
  top.pps = [];
  top.errors := msg;
  top.defs = [];
  top.typerep = errorType();
}

abstract production errorDecl
top::Decl ::= msg::[Message]
{
  top.pp = concat([text("/*"),
    ppImplode(line(), map(text, map((.output), msg))),
    text("*/")]);
  top.errors := msg;
  top.defs = [];
}

nonterminal FunctionDecl with pp, errors, defs, env;

abstract production functionDecl
top::FunctionDecl ::= storage::[StorageClass]  fnquals::[SpecialSpecifier]  bty::BaseTypeExpr mty::TypeModifierExpr  name::Name  decls::Decls  body::Stmt
{
  top.pp = concat([terminate(space(), map((.pp), storage)), terminate( space(), map( (.pp), fnquals ) ),
    bty.pp, space(), mty.lpp, name.pp, mty.rpp, line(), terminate(cat(semi(), line()), decls.pps),
    text("{"), line(), nestlines(2,body.pp), text("}")]);
  
  local parameters :: Decorated Parameters =
    case mty of
    | functionTypeExprWithArgs(result, args, variadic) ->
        args
    | _ -> decorate nilParameters() with { env = top.env; }
    end;
  
  top.errors := bty.errors ++ mty.errors ++ body.errors;
  top.defs = bty.defs ++ [valueDef(name.name, functionValueItem(top))];
  
  body.env = addEnv(top.defs ++ parameters.defs ++ decls.defs ++ body.functiondefs, openScope(addEnv(bty.defs, top.env)));
  
  top.errors <- name.valueRedeclarationCheck;
}

abstract production badFunctionDecl
top::FunctionDecl ::= msg::[Message]
{
  top.pp = concat([text("/*"),
    ppImplode(line(), map(text, map((.output), msg))),
    text("*/")]);
  top.errors := msg;
  top.defs = [];
}

nonterminal Parameters with typereps, pps, errors, defs, env;

abstract production consParameters
top::Parameters ::= h::ParameterDecl  t::Parameters
{
  top.pps = h.pp :: t.pps;
  top.typereps = h.typerep :: t.typereps;
  top.errors := h.errors ++ t.errors;
  top.defs = h.defs ++ t.defs;
  
  t.env = addEnv(h.defs, top.env);
}

abstract production nilParameters
top::Parameters ::=
{
  top.pps = [];
  top.typereps = [];
  top.errors := [];
  top.defs = [];
}

-- TODO: move these, later
synthesized attribute paramname :: Maybe<Name>;

nonterminal ParameterDecl with paramname, typerep, pp, errors, defs, env;

abstract production parameterDecl
top::ParameterDecl ::= storage::[StorageClass]  bty::BaseTypeExpr  mty::TypeModifierExpr  name::MaybeName
{
  top.pp = concat([terminate(space(), map((.pp), storage)),
    bty.pp, space(), mty.lpp, space(), name.pp, mty.rpp]);
  top.paramname = name.maybename;
  top.typerep = mty.typerep;
  mty.baseType = bty.typerep;
  top.errors := bty.errors ++ mty.errors;
  top.defs = bty.defs ++
    case name.maybename of
    | just(n) -> [valueDef(n.name, parameterValueItem(top))]
    | _ -> []
    end;
  
  top.errors <- name.valueRedeclarationCheck;
}


nonterminal StructDecl with pp, maybename, errors, defs, env, tagEnv;

abstract production structDecl
top::StructDecl ::= name::MaybeName  dcls::StructItemList
{
  top.maybename = name.maybename;
  top.pp = concat([text("struct"), space(), name.pp, space(), text("{"),
    nestlines(2, terminate(cat(semi(),line()), dcls.pps)),
    text("}")]);
  top.errors := dcls.errors;

  local thisdcl :: [Def] =
    case name.maybename of
    | just(n) -> [tagDef(n.name, structTagItem(top))]
    | _ -> []
    end;
  top.defs = thisdcl ++ dcls.defs;
  top.tagEnv = addEnv(dcls.localdefs, emptyEnv());
  
  dcls.env = openScope(addEnv(thisdcl, top.env));
  
  top.errors <- name.tagRedeclarationCheck;
}

nonterminal UnionDecl with pp, maybename, errors, defs, env, tagEnv;

abstract production unionDecl
top::UnionDecl ::= name::MaybeName  dcls::StructItemList
{
  top.maybename = name.maybename;
  top.pp = concat([text("union"), space(), name.pp, space(), text("{"),
    nestlines(2, terminate(cat(semi(),line()), dcls.pps)),
    text("}")]);
  top.errors := dcls.errors;

  local thisdcl :: [Def] =
    case name.maybename of
    | just(n) -> [tagDef(n.name, unionTagItem(top))]
    | _ -> []
    end;
  top.defs = thisdcl ++ dcls.defs;
  top.tagEnv = addEnv(dcls.localdefs, emptyEnv());
  
  dcls.env = openScope(addEnv(thisdcl, top.env));
  
  top.errors <- name.tagRedeclarationCheck;
}

nonterminal EnumDecl with pp, maybename, errors, defs, env;

abstract production enumDecl
top::EnumDecl ::= name::MaybeName  dcls::EnumItemList
{
  top.maybename = name.maybename;
  top.pp = concat([text("enum"), space(), name.pp, space(), text("{"),
    nestlines(2, ppImplode(cat(comma(),line()), dcls.pps)),
    text("}")]);
  top.errors := dcls.errors;

  local thisdcl :: [Def] =
    case name.maybename of
    | just(n) -> [tagDef(n.name, enumTagItem(top))]
    | _ -> []
    end;
  top.defs = thisdcl ++ dcls.defs;
  
  dcls.env = addEnv(thisdcl, top.env);

  top.errors <- name.tagRedeclarationCheck;
}


nonterminal StructItemList with pps, errors, defs, env, localdefs;

abstract production nilStructItem
top::StructItemList ::=
{
  top.pps = [];
  top.errors := [];
  top.defs = [];
  top.localdefs = [];
}

abstract production consStructItem
top::StructItemList ::= h::StructItem  t::StructItemList
{
  top.pps = h.pp :: t.pps;
  top.errors := h.errors ++ t.errors;
  top.defs = h.defs ++ t.defs;
  top.localdefs = h.localdefs ++ t.localdefs;
  
  t.env = addEnv(h.defs ++ h.localdefs, h.env);
}

nonterminal EnumItemList with pps, errors, defs, env;

abstract production nilEnumItem
top::EnumItemList ::=
{
  top.pps = [];
  top.errors := [];
  top.defs = [];
}

abstract production consEnumItem
top::EnumItemList ::= h::EnumItem  t::EnumItemList
{
  top.pps = h.pp :: t.pps;
  top.errors := h.errors ++ t.errors;
  top.defs = h.defs ++ t.defs;
}

nonterminal StructItem with pp, errors, defs, env, localdefs;

abstract production structItem
top::StructItem ::= ty::BaseTypeExpr  dcls::StructDeclarators
{
  top.pp = concat([ty.pp, space(), ppImplode(text(", "), dcls.pps)]);
  top.errors := ty.errors ++ dcls.errors;
  top.defs = ty.defs;
  top.localdefs = dcls.localdefs;
}
abstract production warnStructItem
top::StructItem ::= msg::[Message]
{
  top.pp = notext();
  top.errors := msg;
  top.defs = [];
  top.localdefs = [];
}


nonterminal StructDeclarators with pps, errors, localdefs, env;

abstract production consStructDeclarator
top::StructDeclarators ::= h::StructDeclarator  t::StructDeclarators
{
  top.pps = h.pps ++ t.pps;
  top.errors := h.errors ++ t.errors;
  top.localdefs = h.localdefs ++ t.localdefs;
  
  t.env = addEnv(h.localdefs, h.env);
}
abstract production nilStructDeclarator
top::StructDeclarators ::=
{
  top.pps = [];
  top.errors := [];
  top.localdefs = [];
}

nonterminal StructDeclarator with pps, errors, localdefs, env;

abstract production structField
top::StructDeclarator ::= name::Name  ty::TypeModifierExpr
{
  top.pps = [concat([ty.lpp, name.pp, ty.rpp])];
  top.errors := ty.errors;
  top.localdefs = [valueDef(name.name, fieldValueItem(top))];
  
  top.errors <- name.valueRedeclarationCheck;
}
abstract production structBitfield
top::StructDeclarator ::= name::MaybeName  ty::TypeModifierExpr  e::Expr
{
  top.pps = [concat([ty.lpp, name.pp, ty.rpp, text(" : "), e.pp])];
  top.errors := ty.errors ++ e.errors;

  local thisdcl :: [Def] =
    case name.maybename of
    | just(n) -> [valueDef(n.name, fieldValueItem(top))]
    | _ -> []
    end;
  top.localdefs = thisdcl;
  
  top.errors <- name.valueRedeclarationCheck;
}
-- Similar to external declarations, this pretends not to exist if it's only a warning
abstract production warnStructField
top::StructDeclarator ::= msg::[Message]
{
  top.pps = [];
  top.errors := msg;
  top.localdefs = [];
}

nonterminal EnumItem with pp, errors, defs, env;

abstract production enumItem
top::EnumItem ::= name::Name  e::MaybeExpr
{
  top.pp = concat([name.pp] ++ if e.isJust then [text(" = "), e.pp] else []);
  top.errors := e.errors;
  top.defs = [valueDef(name.name, enumValueItem(top))];
  
  top.errors <- name.valueRedeclarationCheck;
}



nonterminal StorageClass with pp;
abstract production externStorageClass
top::StorageClass ::= { top.pp = text("extern"); }
abstract production staticStorageClass
top::StorageClass ::= { top.pp = text("static"); }
abstract production autoStorageClass
top::StorageClass ::= { top.pp = text("auto"); }
abstract production registerStorageClass
top::StorageClass ::= { top.pp = text("register"); }


{-
From clang:

def TranslationUnit : Decl, DeclContext;
def Named : Decl<1>;
  def Label : DDecl<Named>;  - TODO
  def Type : DDecl<Named, 1>;
    def TypedefName : DDecl<Type, 1>;
      def Typedef : DDecl<TypedefName>;   -- done
    def Tag : DDecl<Type, 1>, DeclContext;
      def Enum : DDecl<Tag>;  -- done in typeexpr
      def Record : DDecl<Tag>;  -- ditto
  def Value : DDecl<Named, 1>;
    def EnumConstant : DDecl<Value>;  -- done in typeexpr
    def Declarator : DDecl<Value, 1>;
      def Field : DDecl<Declarator>;  -- done in typeexpr
      def Function : DDecl<Declarator>, DeclContext;  -- done
      def Var : DDecl<Declarator>;  -- done
        def ParmVar : DDecl<Var>;  -- done
def FileScopeAsm : Decl; - ?
def StaticAssert : Decl; - ?
def Block : Decl, DeclContext;
def Captured : Decl, DeclContext;
def Empty : Decl; - ?

Notes: clang likes to have direct references to certain priviledged outer scopes.
e.g. 'getTranslationUnit' and 'getFunction' vs just getting the block scope.
This tends to result in getting things like the list of labels. We may not need that?
If we just have the list in a namespace in scope.


-}

