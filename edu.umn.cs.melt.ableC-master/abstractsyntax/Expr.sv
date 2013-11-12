

nonterminal Expr with location, pp, errors, defs, env;

synthesized attribute integerConstantValue :: Maybe<Integer>;

abstract production errorExpr
top::Expr ::= msg::[Message]
{
  top.pp = concat([ text("/*"), text(messagesToString(msg)), text("*/") ]);
  top.errors := msg;
  top.defs = [];
}
abstract production declRefExpr
top::Expr ::= id::Name
{ -- Reference to a value. (Either a Decl or a EnumItem)
  top.pp = parens( id.pp );
  top.errors := [];
  top.defs = [];
  
  top.errors <- id.valueLookupCheck;
}
abstract production integerLiteral
top::Expr ::= l::String
{ -- TODO: we should probably have some sort of special representation for this
  top.pp = text(l);
  top.errors := [];
  top.defs = [];
}
abstract production floatingLiteral
top::Expr ::= l::String
{
  top.pp = text(l);
  top.errors := [];
  top.defs = [];
}
abstract production stringLiteral
top::Expr ::= l::String
{
  top.pp = text(l);
  top.errors := [];
  top.defs = [];
}
abstract production characterLiteral
top::Expr ::= l::String
{
  top.pp = text(l);
  top.errors := [];
  top.defs = [];
}
abstract production parenExpr
top::Expr ::= e::Expr
{
  top.pp = parens( e.pp );
  top.errors := [];
  top.defs = e.defs;
}
abstract production unaryOpExpr
top::Expr ::= op::UnaryOp  e::Expr
{
  top.pp = if op.preExpr
           then parens( cat( op.pp, e.pp ) )
           else parens( cat( e.pp, op.pp ) );
  top.errors := e.errors;
  top.defs = e.defs;
}
abstract production unaryExprOrTypeTraitExpr
top::Expr ::= op::UnaryTypeOp  e::ExprOrTypeName
{
  top.pp = parens( concat([op.pp,parens(e.pp)]) );
  top.errors := e.errors;
  top.defs = e.defs;
}
abstract production arraySubscriptExpr
top::Expr ::= lhs::Expr  rhs::Expr
{
  top.pp = parens( concat([ lhs.pp, brackets( rhs.pp )]) );
  top.errors := lhs.errors ++ rhs.errors;
  top.defs = lhs.defs ++ rhs.defs;
  
  rhs.env = addEnv(lhs.defs, lhs.env);
}
abstract production callExpr
top::Expr ::= f::Expr  a::Exprs
{
  top.pp = parens( concat([ f.pp, parens( ppImplode( cat( comma(), space() ), a.pps ))]) );
  top.errors := f.errors ++ a.errors;
  top.defs = f.defs ++ a.defs;
  
  a.env = addEnv(f.defs, f.env);
}
abstract production memberExpr
top::Expr ::= lhs::Expr  deref::Boolean  rhs::Name
{
  top.pp = if deref 
           then parens( concat([lhs.pp, text("->"), rhs.pp]) ) 
           else parens( concat([lhs.pp, text("."), rhs.pp]) );
  top.errors := lhs.errors;
  top.defs = lhs.defs;
}
abstract production binaryOpExpr
top::Expr ::= lhs::Expr  op::BinOp  rhs::Expr
{
  top.pp = parens( concat([ lhs.pp, space(), op.pp, space(), rhs.pp ]) );
  top.errors := lhs.errors ++ rhs.errors;
  top.defs = lhs.defs ++ rhs.defs;
  
  rhs.env = addEnv(lhs.defs, lhs.env);
}
abstract production conditionalExpr
top::Expr ::= cond::Expr  t::Expr  e::Expr
{
  top.pp = parens( concat([ cond.pp, space(), text("?"), space(), t.pp, space(), text(":"),  space(), e.pp]) );
  top.errors := cond.errors ++ t.errors ++ e.errors;
  top.defs = cond.defs ++ t.defs ++ e.defs;
  
  t.env = addEnv(cond.defs, cond.env);
  e.env = addEnv(t.defs, t.env);
}
abstract production explicitCastExpr
top::Expr ::= ty::TypeName  e::Expr
{
  top.pp = parens( concat([parens(ty.pp), e.pp]) );
  top.errors := ty.errors ++ e.errors;
  top.defs = ty.defs ++ e.defs;
  
  e.env = addEnv(ty.defs, ty.env);
}
abstract production compoundLiteralExpr
top::Expr ::= ty::TypeName  init::InitList
{
  top.pp = parens( concat([parens(ty.pp), text("{"), ppImplode(text(", "), init.pps), text("}")]) );
  top.errors := ty.errors ++ init.errors;
  top.defs = ty.defs ++ init.defs;
  
  init.env = addEnv(ty.defs, ty.env);
}
abstract production predefinedFuncExpr
top::Expr ::= 
{ -- Currently (C99) just __func__ in functions.
  top.pp = parens( text("__func__") );
  top.errors := [];
  top.defs = [];
}


{- from clang:

abstract production imaginaryLiteral
top::Expr ::= l::String
{ -- TODO: gcc
}

// Expressions
def Expr : Stmt<1>;
def PredefinedExpr : DStmt<Expr>;          __func__ in C99
def DeclRefExpr : DStmt<Expr>;             either a Decl or a EnumItem at the moment
def IntegerLiteral : DStmt<Expr>;
def FloatingLiteral : DStmt<Expr>;
def ImaginaryLiteral : DStmt<Expr>;
def StringLiteral : DStmt<Expr>;
def CharacterLiteral : DStmt<Expr>;
def ParenExpr : DStmt<Expr>;
def UnaryOperator : DStmt<Expr>;             except sizeof and alignof
def OffsetOfExpr : DStmt<Expr>;
def UnaryExprOrTypeTraitExpr : DStmt<Expr>;  sizeof and alignof  -- need some sort of 'expr or type' nonterminal. or two prods
def ArraySubscriptExpr : DStmt<Expr>;
def CallExpr : DStmt<Expr>;
def MemberExpr : DStmt<Expr>;                both -> and .
def CastExpr : DStmt<Expr, 1>;               
def BinaryOperator : DStmt<Expr>;            all ops and comparisons
def CompoundAssignOperator : DStmt<BinaryOperator>;     assign-ops. 
def AbstractConditionalOperator : DStmt<Expr, 1>;
def ConditionalOperator : DStmt<AbstractConditionalOperator>;        normal ?:
def BinaryConditionalOperator : DStmt<AbstractConditionalOperator>;  GNU missing-middle ?:
def ImplicitCastExpr : DStmt<CastExpr>;      TODO: we may need to insert these into the ast. Seems to do things like implicit conversion
def ExplicitCastExpr : DStmt<CastExpr, 1>;   Clang has subtypes, but this will always be a '(type)expr' I think
def CStyleCastExpr : DStmt<ExplicitCastExpr>;
def CompoundLiteralExpr : DStmt<Expr>;       this is (struct foo){initializer}
def ExtVectorElementExpr : DStmt<Expr>;      don't care
def InitListExpr : DStmt<Expr>;              Goes to an initializer
def DesignatedInitExpr : DStmt<Expr>;        C99 designated initializer
def ImplicitValueInitExpr : DStmt<Expr>;     I guess implicit zeros for an initializer?
def ParenListExpr : DStmt<Expr>;             TODO: no idea?
def VAArgExpr : DStmt<Expr>;                 __builtin_var_arg
def GenericSelectionExpr : DStmt<Expr>;      _Generic C11
def PseudoObjectExpr : DStmt<Expr>;          don't think we care?

// Atomic expressions
def AtomicExpr : DStmt<Expr>;

// GNU Extensions.
def AddrLabelExpr : DStmt<Expr>;
def StmtExpr : DStmt<Expr>;
def ChooseExpr : DStmt<Expr>;
def GNUNullExpr : DStmt<Expr>;

// Clang Extensions.
def ShuffleVectorExpr : DStmt<Expr>;
def BlockExpr : DStmt<Expr>;
def OpaqueValueExpr : DStmt<Expr>;

-}
