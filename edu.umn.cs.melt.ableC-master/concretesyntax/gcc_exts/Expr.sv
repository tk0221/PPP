grammar edu:umn:cs:melt:ableC:concretesyntax:gcc_exts;

terminal GNU_AlignOf_T '__alignof' lexer classes {Ckeyword};
terminal GNU_AlignOfUU_T '__alignof__' lexer classes {Ckeyword};

terminal GNU_BuiltinVaArg_t '__builtin_va_arg' lexer classes {Ckeyword};
terminal GNU_BuiltinOffsetof_t '__builtin_offsetof' lexer classes {Ckeyword};
terminal GNU_BuiltinTypesCompatible_t '__builtin_types_compatible_p' lexer classes {Ckeyword};

-- If not null, otherwise
concrete productions top::ConditionalExpr_c
| c::LogicalOrExpr_c '?' ':' e::ConditionalExpr_c
    { top.ast = ast:binaryConditionalExpr(c.ast, e.ast, location=top.location); }

concrete productions top::UnaryExpr_c
| '__alignof' e::UnaryExpr_c
    { top.ast = ast:unaryExprOrTypeTraitExpr(ast:alignofOp(location=$1.location), ast:exprExpr(e.ast), location=top.location); }
| '__alignof' '(' t::TypeName_c ')'
    { top.ast = ast:unaryExprOrTypeTraitExpr(ast:alignofOp(location=$1.location), ast:typeNameExpr(t.ast), location=top.location); }
| '__alignof__' e::UnaryExpr_c
    { top.ast = ast:unaryExprOrTypeTraitExpr(ast:alignofOp(location=$1.location), ast:exprExpr(e.ast), location=top.location); }
| '__alignof__' '(' t::TypeName_c ')'
    { top.ast = ast:unaryExprOrTypeTraitExpr(ast:alignofOp(location=$1.location), ast:typeNameExpr(t.ast), location=top.location); }
| '&&' Identifier_t
    { top.ast = ast:errorExpr([err(top.location, "Address of labels not yet supported")], location=top.location); }

concrete productions top::UnaryOp_c
| '__extension__'
    { top.ast = ast:warnNoOp([wrn($1.location, "Ignoring __extension__")], location=top.location); }

concrete productions top::PrimaryExpr_c 
| '(' s::CompoundStatement_c ')'
    { top.ast = ast:stmtExpr(s.ast, location=top.location); }
| '__builtin_va_arg' '(' e::AssignExpr_c ',' ty::TypeName_c ')'
    { top.ast = ast:vaArgExpr(e.ast, ty.ast, location=top.location); }
| '__builtin_offsetof' '(' ts::TypeName_c ',' e::MemberDesignator_c ')'
    { top.ast = ast:offsetofExpr(ts.ast, e.ast, location=top.location); }
| '__builtin_types_compatible_p' '(' t1::TypeName_c ',' t2::TypeName_c ')'
    { top.ast = ast:typesCompatibleExpr(t1.ast, t2.ast, location=top.location); }

closed nonterminal MemberDesignator_c with location, ast<ast:MemberDesignator>;
concrete productions top::MemberDesignator_c
| id::Identifier_t
    { top.ast = ast:initialMemberDesignator(ast:fromId(id)); }
| d::MemberDesignator_c '.' id::Identifier_t
    { top.ast = ast:fieldMemberDesignator(d.ast, ast:fromId(id)); }
| d::MemberDesignator_c '->' id::Identifier_t
    { top.ast = ast:derefMemberDesignator(d.ast, ast:fromId(id)); }
| d::MemberDesignator_c '[' e::ConstantExpr_c ']'
    { top.ast = ast:arrayMemberDesignator(d.ast, e.ast); }

concrete productions top::Initializer_c
| '{' '}'
    { top.ast = ast:objectInitializer(ast:nilInit()); }

concrete productions top::Designation_c
| d::ArrayDesignator_c
    { top.ast = d.ast;
      d.givenDesignator = ast:initialDesignator(); }
| id::Identifier_t ':'
    { top.ast = ast:fieldDesignator(ast:initialDesignator(), ast:fromId(id)); }

concrete productions top::ArrayDesignator_c
| '[' e1::ConstantExpr_c '...' e2::ConstantExpr_c ']'
    { top.ast = ast:arrayRangeDesignator(top.givenDesignator, e1.ast, e2.ast); }


