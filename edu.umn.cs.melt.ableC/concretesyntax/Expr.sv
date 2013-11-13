grammar edu:umn:cs:melt:ableC:concretesyntax;

-- "Exported" nonterminals

closed nonterminal Expr_c with location, ast<ast:Expr>; 
concrete productions top::Expr_c
| e::AssignExpr_c
    { top.ast = e.ast; }
| l::AssignExpr_c ',' r::Expr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:commaOp(location=$2.location), r.ast, location=top.location); }


closed nonterminal AssignExpr_c with location, ast<ast:Expr>; 
concrete productions top::AssignExpr_c
| e::ConditionalExpr_c
    { top.ast = e.ast; }
| l::UnaryExpr_c op::AssignOp_c  r::AssignExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:assignOp(op.ast, location=op.location), r.ast, location=top.location); }


closed nonterminal ConstantExpr_c with location, ast<ast:Expr>;
concrete productions top::ConstantExpr_c
| e::ConditionalExpr_c
    { top.ast = e.ast; }


closed nonterminal Initializer_c with location, ast<ast:Initializer>; 
concrete productions top::Initializer_c
| e::AssignExpr_c
    { top.ast = ast:exprInitializer(e.ast); }
| '{' il::InitializerList_c '}'
    { top.ast = ast:objectInitializer(ast:foldInit(il.ast)); }
| '{' il::InitializerList_c ',' '}' 
    { top.ast = ast:objectInitializer(ast:foldInit(il.ast)); }


-- "Non-exported" nonterminals


closed nonterminal AssignOp_c with location, ast<ast:AssignOp>;
concrete productions top::AssignOp_c
| '='   { top.ast = ast:eqOp(location=top.location); }
| '*='  { top.ast = ast:mulEqOp(location=top.location); }
| '/='  { top.ast = ast:divEqOp(location=top.location); }
| '%='  { top.ast = ast:modEqOp(location=top.location); }
| '+='  { top.ast = ast:addEqOp(location=top.location); }
| '-='  { top.ast = ast:subEqOp(location=top.location); }
| '<<=' { top.ast = ast:lshEqOp(location=top.location); }
| '>>=' { top.ast = ast:rshEqOp(location=top.location); }
| '&='  { top.ast = ast:andEqOp(location=top.location); }
| '^='  { top.ast = ast:xorEqOp(location=top.location); }
| '|='  { top.ast = ast:orEqOp(location=top.location); }


closed nonterminal ConditionalExpr_c with location, ast<ast:Expr>;
concrete productions top::ConditionalExpr_c
| ce::LogicalOrExpr_c  '?' te::Expr_c  ':'  ee::ConditionalExpr_c
    { top.ast = ast:conditionalExpr(ce.ast, te.ast, ee.ast, location=top.location); }
| e::LogicalOrExpr_c
    { top.ast = e.ast; }


closed nonterminal LogicalOrExpr_c with location, ast<ast:Expr>;
concrete productions top::LogicalOrExpr_c
| e::LogicalAndExpr_c
    { top.ast = e.ast; }
| l::LogicalOrExpr_c '||' r::LogicalAndExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:boolOp(ast:orBoolOp(location=$2.location), location=$2.location), r.ast, location=top.location); }


closed nonterminal LogicalAndExpr_c with location, ast<ast:Expr>;
concrete productions top::LogicalAndExpr_c
| e::InclusiveOrExpr_c
    { top.ast = e.ast; }
| l::LogicalAndExpr_c '&&' r::InclusiveOrExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:boolOp(ast:andBoolOp(location=$2.location), location=$2.location), r.ast, location=top.location); }


closed nonterminal InclusiveOrExpr_c with location, ast<ast:Expr>;
concrete productions top::InclusiveOrExpr_c
| e::ExclusiveOrExpr_c
    { top.ast = e.ast; }
| l::InclusiveOrExpr_c '|' r::ExclusiveOrExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:bitOp(ast:orBitOp(location=$2.location), location=$2.location), r.ast, location=top.location); }


closed nonterminal ExclusiveOrExpr_c with location, ast<ast:Expr>;
concrete productions top::ExclusiveOrExpr_c
| e::AndExpr_c
    { top.ast = e.ast; }
| l::ExclusiveOrExpr_c '^' r::AndExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:bitOp(ast:xorBitOp(location=$2.location), location=$2.location), r.ast, location=top.location); }


closed nonterminal AndExpr_c with location, ast<ast:Expr>;
concrete productions top::AndExpr_c
| e::EqualityExpr_c
    { top.ast = e.ast; }
| l::AndExpr_c '&' r::EqualityExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:bitOp(ast:andBitOp(location=$2.location), location=$2.location), r.ast, location=top.location); }


closed nonterminal EqualityExpr_c with location, ast<ast:Expr>;
concrete productions top::EqualityExpr_c
| e::RelationalExpr_c
    { top.ast = e.ast; }
| l::EqualityExpr_c '==' r::RelationalExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:compareOp(ast:equalsOp(location=$2.location), location=$2.location), r.ast, location=top.location); }
| l::EqualityExpr_c '!=' r::RelationalExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:compareOp(ast:notEqualsOp(location=$2.location), location=$2.location), r.ast, location=top.location); }


closed nonterminal RelationalExpr_c with location, ast<ast:Expr>;
concrete productions top::RelationalExpr_c
| e::ShiftExpr_c
    { top.ast = e.ast; }
| l::RelationalExpr_c '<' r::ShiftExpr_c 
    { top.ast = ast:binaryOpExpr(l.ast, ast:compareOp(ast:ltOp(location=$2.location), location=$2.location), r.ast, location=top.location); }
| l::RelationalExpr_c '>' r::ShiftExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:compareOp(ast:gtOp(location=$2.location), location=$2.location), r.ast, location=top.location); }
| l::RelationalExpr_c '<=' r::ShiftExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:compareOp(ast:lteOp(location=$2.location), location=$2.location), r.ast, location=top.location); }
| l::RelationalExpr_c '>=' r::ShiftExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:compareOp(ast:gteOp(location=$2.location), location=$2.location), r.ast, location=top.location); }


closed nonterminal ShiftExpr_c with location, ast<ast:Expr>;
concrete productions top::ShiftExpr_c
| e::AdditiveExpr_c
    { top.ast = e.ast; }
| l::ShiftExpr_c '<<' r::AdditiveExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:bitOp(ast:lshBitOp(location=$2.location), location=$2.location), r.ast, location=top.location); }
| l::ShiftExpr_c '>>' r::AdditiveExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:bitOp(ast:rshBitOp(location=$2.location), location=$2.location), r.ast, location=top.location); }


-- Additive Expressions --
--------------------------
closed nonterminal AdditiveExpr_c with location, ast<ast:Expr>;
{- Below is the previous implementation of AdditiveExpr_c.  
concrete productions top::AdditiveExpr_c
| e::MultiplicativeExpr_c
    { top.ast = e.ast; }
| l::AdditiveExpr_c  '+'  r::MultiplicativeExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:numOp(ast:addOp(location=$2.location), location=$2.location), r.ast, location=top.location); }
| l::AdditiveExpr_c  '-'  r::MultiplicativeExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:numOp(ast:subOp(location=$2.location), location=$2.location), r.ast, location=top.location); }           -}
concrete productions top::AdditiveExpr_c
| e::AddMulLeft_c 
    { top.ast = e.ast; }
| l::AdditiveExpr_c  op::AdditiveOp_c  r::AddMulLeft_c 
    { top.ast = op.ast; 
      op.leftExpr=l.ast; op.rightExpr=r.ast; op.exprLocation=top.location; }

inherited attribute leftExpr :: ast:Expr;
inherited attribute rightExpr :: ast:Expr;
inherited attribute exprLocation :: Location;

closed nonterminal AdditiveOp_c 
  with location, ast<ast:Expr>, leftExpr, rightExpr, exprLocation ;

-- Additive Operators
concrete productions top::AdditiveOp_c
| '+'
    { top.ast = ast:binaryOpExpr(top.leftExpr, 
        ast:numOp(ast:addOp(location=$1.location), location=$1.location), 
        top.rightExpr, location=top.exprLocation); }
| '-'
    { top.ast = ast:binaryOpExpr(top.leftExpr, 
        ast:numOp(ast:subOp(location=$1.location), location=$1.location),
        top.rightExpr, location=top.exprLocation); }

-- Operators with precedence between Additive and Multiplicitive opererators

-- Left Associative
closed nonterminal AddMulLeft_c with location, ast<ast:Expr> ;
concrete productions top::AddMulLeft_c
| e::AddMulRight_c
    { top.ast = e.ast; }
| l::AddMulLeft_c  op::AddMulLeftOp_c r::AddMulRight_c
    { top.ast = op.ast; 
      op.leftExpr=l.ast; op.rightExpr=r.ast; op.exprLocation=top.location; }

closed nonterminal AddMulLeftOp_c
  with location, ast<ast:Expr>, leftExpr, rightExpr, exprLocation ;

terminal AddMulLeft_NEVER_t 'AddMulLeft_Never!!!nevernever1234567890' ;
concrete productions top::AddMulLeftOp_c
| AddMulLeft_NEVER_t
    { top.ast = ast:errorExpr ( [ err (top.location, "Internal Error. " ++
        "Placeholder for AddMulLeftOp_c should not appear in the tree.") ],
        location=top.location ) ; }

-- Right Associative
closed nonterminal AddMulRight_c with location, ast<ast:Expr> ;
concrete productions top::AddMulRight_c
| e::AddMulNone_c
    { top.ast = e.ast; }
| l::AddMulNone_c  op::AddMulRightOp_c r::AddMulRight_c 
    { top.ast = op.ast; 
      op.leftExpr=l.ast; op.rightExpr=r.ast; op.exprLocation=top.location; }

closed nonterminal AddMulRightOp_c
  with location, ast<ast:Expr>, leftExpr, rightExpr, exprLocation ;

terminal AddMulRight_NEVER_t 'AddMulRight_Never!!!nevernever1234567890' ;
concrete productions top::AddMulRightOp_c
| AddMulRight_NEVER_t
    { top.ast = ast:errorExpr ( [ err (top.location, "Internal Error. " ++
        "Placeholder for AddMulRigthOp_c should not appear in the tree." ) ],
        location=top.location ) ; }

-- Non-associative
closed nonterminal AddMulNone_c with location, ast<ast:Expr> ;
concrete productions top::AddMulNone_c
| e::MultiplicativeExpr_c
    { top.ast = e.ast; }
| l::MultiplicativeExpr_c  op::AddMulNoneOp_c r::MultiplicativeExpr_c 
    { top.ast = op.ast; 
      op.leftExpr=l.ast; op.rightExpr=r.ast; op.exprLocation=top.location; }

closed nonterminal AddMulNoneOp_c
  with location, ast<ast:Expr>, leftExpr, rightExpr, exprLocation ;

terminal AddMulNone_NEVER_t 'AddMulNone_Never!!!nevernever1234567890' ;
concrete productions top::AddMulNoneOp_c
| AddMulNone_NEVER_t
    { top.ast = ast:errorExpr ( [ err (top.location, "Internal Error. " ++
        "Placeholder for AddMulNoneOp_c should not appear in the tree." ) ],
        location=top.location ) ; }


-- Multiplicative Expressions --
--------------------------------
closed nonterminal MultiplicativeExpr_c with location, ast<ast:Expr>;
concrete productions top::MultiplicativeExpr_c
| e::CastExpr_c
    { top.ast = e.ast; }
| l::MultiplicativeExpr_c '*' r::CastExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:numOp(ast:mulOp(location=$2.location), 
        location=$2.location), r.ast, location=top.location); }
| l::MultiplicativeExpr_c '/' r::CastExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:numOp(ast:divOp(location=$2.location),
        location=$2.location), r.ast, location=top.location); }
| l::MultiplicativeExpr_c '%' r::CastExpr_c
    { top.ast = ast:binaryOpExpr(l.ast, ast:numOp(ast:modOp(location=$2.location),
        location=$2.location), r.ast, location=top.location); }


closed nonterminal CastExpr_c with location, ast<ast:Expr>;
concrete productions top::CastExpr_c
| e::UnaryExpr_c
    { top.ast = e.ast; }
| '(' tn::TypeName_c ')' e::CastExpr_c
    { top.ast = ast:explicitCastExpr(tn.ast, e.ast, location=top.location); }


closed nonterminal UnaryExpr_c with location, ast<ast:Expr>;
concrete productions top::UnaryExpr_c
| e::PostfixExpr_c
    { top.ast = e.ast; }
| '++' e::UnaryExpr_c
    { top.ast = ast:unaryOpExpr(ast:preIncOp(location=$1.location), e.ast, location=top.location); }
| '--' e::UnaryExpr_c
    { top.ast = ast:unaryOpExpr(ast:preDecOp(location=$1.location), e.ast, location=top.location); }
| op::UnaryOp_c e::CastExpr_c
    { top.ast = ast:unaryOpExpr(op.ast, e.ast, location=top.location); }
| 'sizeof' e::UnaryExpr_c
    { top.ast = ast:unaryExprOrTypeTraitExpr(ast:sizeofOp(location=$1.location), ast:exprExpr(e.ast), location=top.location); }
 | 'sizeof' '(' ty::TypeName_c ')'
    { top.ast = ast:unaryExprOrTypeTraitExpr(ast:sizeofOp(location=$1.location), ast:typeNameExpr(ty.ast), location=top.location); }


closed nonterminal UnaryOp_c with location, ast<ast:UnaryOp>;
concrete productions top::UnaryOp_c
| '&'  { top.ast = ast:addressOfOp(location=top.location); }
| '*'  { top.ast = ast:dereferenceOp(location=top.location); }
| '+'  { top.ast = ast:positiveOp(location=top.location); }
| '-'  { top.ast = ast:negativeOp(location=top.location); }
| '~'  { top.ast = ast:bitNegateOp(location=top.location); }
| '!'  { top.ast = ast:notOp(location=top.location); }


closed nonterminal PostfixExpr_c with location, ast<ast:Expr>;
concrete productions top::PostfixExpr_c
| e::PrimaryExpr_c
    { top.ast = e.ast; }
| e::PostfixExpr_c '[' index::Expr_c ']'
    { top.ast = ast:arraySubscriptExpr(e.ast, index.ast, location=top.location); }
| e::PostfixExpr_c '(' args::ArgumentExprList_c ')'
    { top.ast = ast:callExpr(e.ast, ast:foldExpr(args.ast), location=top.location); }
| e::PostfixExpr_c '(' ')'
    { top.ast = ast:callExpr(e.ast, ast:nilExpr(), location=top.location); }
| e::PostfixExpr_c '.' id::Identifier_t
    { top.ast = ast:memberExpr(e.ast, false, ast:fromId(id), location=top.location); }
| e::PostfixExpr_c '->' id::Identifier_t
    { top.ast = ast:memberExpr(e.ast, true, ast:fromId(id), location=top.location); }
| e::PostfixExpr_c '++'
    { top.ast = ast:unaryOpExpr(ast:postIncOp(location=$2.location), e.ast, location=top.location); }
| e::PostfixExpr_c '--'
    { top.ast = ast:unaryOpExpr(ast:postDecOp(location=$2.location), e.ast, location=top.location); }
| '(' ty::TypeName_c ')' '{' il::InitializerList_c '}'
    { top.ast = ast:compoundLiteralExpr(ty.ast, ast:foldInit(il.ast), location=top.location); }
| '(' ty::TypeName_c ')' '{' il::InitializerList_c ',' '}'
    { top.ast = ast:compoundLiteralExpr(ty.ast, ast:foldInit(il.ast), location=top.location); }


closed nonterminal ArgumentExprList_c with location, ast<[ast:Expr]>;
concrete productions top::ArgumentExprList_c
| e::AssignExpr_c
    { top.ast = [e.ast]; }
| h::ArgumentExprList_c ',' t::AssignExpr_c
    { top.ast = h.ast ++ [t.ast]; }


closed nonterminal PrimaryExpr_c with location, ast<ast:Expr>;
concrete productions top::PrimaryExpr_c
| id::Identifier_t
    { top.ast = ast:declRefExpr(ast:fromId(id), location=top.location); }
| dc::DecimalConstant_t
    { top.ast = ast:integerLiteral(dc.lexeme, location=top.location); }
| fc::FloatConstant_t
    { top.ast = ast:floatingLiteral(fc.lexeme, location=top.location); }
| hf::HexFloatConst_t
    { top.ast = ast:floatingLiteral(hf.lexeme, location=top.location); }
| c::Constant_t
    { top.ast = ast:integerLiteral(c.lexeme, location=top.location); }
| sl::StringConstant_c
    { top.ast = ast:stringLiteral(sl.ast, location=top.location); }
| cl::CharLiteral_t
    { top.ast = ast:characterLiteral(cl.lexeme, location=top.location); }
| '(' e::Expr_c  ')'
    { top.ast = ast:parenExpr(e.ast, location=top.location); }


closed nonterminal StringConstant_c with location, ast<String>; -- This is a bit weird.
concrete productions top::StringConstant_c
| sl::StringLiteral_t
    { top.ast = sl.lexeme; }
| h::StringLiteral_t  t::StringConstant_c
    { top.ast = 
        substring(0, length(h.lexeme) - 1, h.lexeme) ++
        substring(1, length(t.ast), t.ast); }


closed nonterminal InitializerList_c with location, ast<[ast:Init]>;
concrete productions top::InitializerList_c
| i::Initializer_c 
    { top.ast = [ast:init(i.ast)]; }
| d::Designation_c  i::Initializer_c 
    { top.ast = [ast:designatedInit(d.ast, i.ast)]; }
| il::InitializerList_c ',' i::Initializer_c
    { top.ast = il.ast ++ [ast:init(i.ast)]; }
| il::InitializerList_c ',' d::Designation_c  i::Initializer_c
    { top.ast = il.ast ++ [ast:designatedInit(d.ast, i.ast)]; }


closed nonterminal Designation_c with location, ast<ast:Designator>;
concrete productions top::Designation_c
| d::DesignatorList_c '='
    { top.ast = d.ast;
      d.givenDesignator = ast:initialDesignator(); }


closed nonterminal DesignatorList_c with location, ast<ast:Designator>, givenDesignator;
concrete productions top::DesignatorList_c
| h::DesignatorList_c  t::Designator_c
    { top.ast = t.ast;
      t.givenDesignator = h.ast; }
| d::Designator_c
    { top.ast = d.ast; }

-- The previous designator to operate upon.
autocopy attribute givenDesignator :: ast:Designator;

closed nonterminal Designator_c with location, ast<ast:Designator>, givenDesignator;
concrete productions top::Designator_c
| d::ArrayDesignator_c
    { top.ast = d.ast; }
| '.' id::Identifier_t
    { top.ast = ast:fieldDesignator(top.givenDesignator, ast:fromId(id)); }


-- This Nt not strictly part of C99. Exists for ease of extensions.
closed nonterminal ArrayDesignator_c with location, ast<ast:Designator>, givenDesignator;
concrete productions top::ArrayDesignator_c
| '[' e::ConstantExpr_c ']'
    { top.ast = ast:arrayDesignator(top.givenDesignator, e.ast); }

