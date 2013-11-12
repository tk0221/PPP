grammar edu:umn:cs:melt:ableC:concretesyntax:gcc_exts;

imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as ast;
imports silver:langutil;

terminal CPP_Extension_t '__extension__' lexer classes {Ckeyword};
terminal CPP_Inline_OneSided_t '__inline' lexer classes {Ckeyword};
terminal CPP_Inline_t '__inline__' lexer classes {Ckeyword};
terminal CPP_Signed_t '__signed__' lexer classes {Ckeyword};
terminal CPP_Volatile_t '__volatile__' lexer classes {Ckeyword};
terminal CPP_UUVolatile_t '__volatile' lexer classes {Ckeyword};
terminal UUConst_t '__const' lexer classes { Ckeyword };
terminal UUBuiltinVAList_t '__builtin_va_list' lexer classes {Ckeyword};
terminal UURestrict_t '__restrict' lexer classes { Ckeyword };
terminal UURestrictUU_t '__restrict__' lexer classes { Ckeyword };
terminal Typeof_t 'typeof' lexer classes {Ckeyword};
terminal CPP_Typeof_t '__typeof__' lexer classes {Ckeyword};
terminal CPP_UUTypeof_t '__typeof' lexer classes {Ckeyword};

concrete productions top::ExternalDeclaration_c
| ';'
    { top.ast = []; }
| '__extension__' d::ExternalDeclaration_c
    { top.ast = [ast:warnExternalDecl([wrn($1.location, "Ignored __extension__")])] ++ d.ast; }
| s::SimpleAsmStatement_c
    { top.ast = [ast:warnExternalDecl([wrn($1.location, "Ignored top-level asm declaration")])]; }

concrete productions top::FunctionSpecifier_c
| '__inline__'
    { top.specialSpecifiers = [ast:inlineQualifier()]; }
| '__inline'
    { top.specialSpecifiers = [ast:inlineQualifier()]; }

concrete productions top::TypeQualifier_c
| '__const'
    { top.typeQualifiers = [ast:constQualifier()];
      top.mutateTypeSpecifiers = []; }
| '__restrict'
    { top.typeQualifiers = [ast:restrictQualifier()];
      top.mutateTypeSpecifiers = []; }
| '__restrict__'
    { top.typeQualifiers = [ast:restrictQualifier()];
      top.mutateTypeSpecifiers = []; }
| '__volatile__'
    { top.typeQualifiers = [ast:volatileQualifier()];
      top.mutateTypeSpecifiers = []; }
| '__volatile'
    { top.typeQualifiers = [ast:volatileQualifier()];
      top.mutateTypeSpecifiers = []; }

concrete productions top::TypeSpecifier_c
| '__builtin_va_list'
    { top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [ast:vaListTypeExpr()]; }
| '__signed__'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["signed"]; }
| t::TypeofStarter_c '(' ts::TypeName_c ')' 
    { top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [ast:typeofTypeExpr(top.givenQualifiers, ast:typeNameExpr(ts.ast))]; } -- TODO: bug: qualifiers get attached to what? I suppose here and all
| t::TypeofStarter_c '(' e::Expr_c ')'
    { top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [ast:typeofTypeExpr(top.givenQualifiers, ast:exprExpr(e.ast))]; }

closed nonterminal TypeofStarter_c with location;
concrete productions top::TypeofStarter_c
| 'typeof' {}
| '__typeof__' {}
| '__typeof' {}

concrete productions top::StructDeclaration_c
| '__extension__' d::StructDeclaration_c
    { top.ast = ast:warnStructItem([wrn($1.location, "Ignored __extension__")]) :: d.ast; }

-- We need a separate nonterminal for this because we're *requiring* the initial DeclSpecs, to avoid ambiguity.
-- We also don't allow old-style DeclarationLists.
closed nonterminal NestedFunctionDefinition_c with location, ast<ast:FunctionDecl>;
concrete productions top::NestedFunctionDefinition_c
| d::InitialNestedFunctionDefinition_c  s::CompoundStatement_c 
    { top.ast = d.ast;
      d.givenStmt = s.ast;
    }
    action {
      context = closeScope(context); -- Opened by InitialNestedFunctionDefinition.
    }
closed nonterminal InitialNestedFunctionDefinition_c with location, ast<ast:FunctionDecl>, givenStmt;
concrete productions top::InitialNestedFunctionDefinition_c
| ds::DeclarationSpecifiers_c  d::Declarator_c
    {
      ds.givenQualifiers = ds.typeQualifiers;
      d.givenType = ast:baseTypeExpr();

      local bt :: ast:BaseTypeExpr =
        ast:figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);

      top.ast = 
        ast:functionDecl(ds.storageClass, ds.specialSpecifiers, bt, d.ast, d.declaredIdent, ast:foldDecl([]), top.givenStmt);
    }
    action {
      -- TODO: we have to duplicate this more. yaaay...
      context = beginFunctionScope(d.declaredIdent, d.declaredParamIdents, context);
    }

concrete productions top::InitialFunctionDefinition_c
| ds::DeclarationSpecifiers_c  d::Declarator_c  aa::Attributes_c
    {
      ds.givenQualifiers = ds.typeQualifiers;
      d.givenType = ast:baseTypeExpr();
      
      local bt :: ast:BaseTypeExpr =
        ast:figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);

      top.ast = 
        ast:functionDecl(ds.storageClass, ds.specialSpecifiers, bt, d.ast, d.declaredIdent, ast:foldDecl([]), top.givenStmt);
      -- TODO: attach attributes somehow
    }
| d::Declarator_c aa::Attributes_c
    {
      top.ast = ast:badFunctionDecl([err(top.location, "Ignoring odd old-style attributed function decl")]);
      -- lol let's not even bother with this TODO future
    }

concrete productions top::InitDeclarator_c
| d::Declarator_c  aa::Attributes_c
    precedence = 20 -- See DeclarationSpecifiers below.
    { top.declaredIdent = d.declaredIdent;
      d.givenType = ast:baseTypeExpr();
      top.ast =
        [ast:errorDeclarator([wrn(aa.location, "Ignoring attribute")]),
         ast:declarator(d.declaredIdent, d.ast, ast:nothingInitializer())];
    }
| d::Declarator_c  aa::Attributes_c  '='  i::Initializer_c
    { top.declaredIdent = d.declaredIdent;
      d.givenType = ast:baseTypeExpr();
      top.ast =
        [ast:errorDeclarator([wrn(aa.location, "Ignoring attribute")]),
         ast:declarator(d.declaredIdent, d.ast, ast:justInitializer(i.ast))];
    }
| d::Declarator_c  a::SimpleAsmStatement_c
    operator=Cpp_Attribute_high_prec
    { top.declaredIdent = d.declaredIdent;
      d.givenType = ast:baseTypeExpr();
      top.ast =
        [ast:errorDeclarator([wrn(a.location, "Ignoring asm annotation")]),
         ast:declarator(d.declaredIdent, d.ast, ast:nothingInitializer())];
    }
| d::Declarator_c  a::SimpleAsmStatement_c  aa::Attributes_c
    operator=Cpp_Attribute_high_prec
    { top.declaredIdent = d.declaredIdent;
      d.givenType = ast:baseTypeExpr();
      top.ast =
        [ast:errorDeclarator([wrn(a.location, "Ignoring asm annotation")]),
         ast:errorDeclarator([wrn(aa.location, "Ignoring attribute")]),
         ast:declarator(d.declaredIdent, d.ast, ast:nothingInitializer())];
    }

concrete productions top::DeclarationSpecifiers_c
| h::Attributes_c  t::DeclarationSpecifiers_c
    { top.isTypedef = t.isTypedef;
      top.storageClass = t.storageClass;
      top.preTypeSpecifiers = t.preTypeSpecifiers;
      top.realTypeSpecifiers = t.realTypeSpecifiers;
      top.typeQualifiers = t.typeQualifiers;
      top.specialSpecifiers = h.specialSpecifiers ++ t.specialSpecifiers;
      top.mutateTypeSpecifiers = t.mutateTypeSpecifiers; }
| h::Attributes_c
    precedence = 10 -- See InitDeclarator above.
    -- Looking at a function decl: DeclarationSpecifiers Declarator . DeclarationList CompoundStatement
    -- We choose to make it part of the declarator, always, not as a DeclSpec for the DeclarationList.
    { top.isTypedef = false;
      top.storageClass = [];
      top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [];
      top.typeQualifiers = [];
      top.specialSpecifiers = h.specialSpecifiers;
      top.mutateTypeSpecifiers = []; }

concrete productions top::SpecifierQualifierList_c
| h::Attributes_c  t::SpecifierQualifierList_c
    { top.preTypeSpecifiers = t.preTypeSpecifiers;
      top.realTypeSpecifiers = t.realTypeSpecifiers;
      top.typeQualifiers = t.typeQualifiers;
      top.mutateTypeSpecifiers = t.mutateTypeSpecifiers;
      top.specialSpecifiers = h.specialSpecifiers ++ t.specialSpecifiers; }
| h::Attributes_c 
    { top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [];
      top.typeQualifiers = [];
      top.mutateTypeSpecifiers = [];
      top.specialSpecifiers = h.specialSpecifiers; }

concrete productions top::TypeQualifierList_c
| h::Attributes_c  t::TypeQualifierList_c
    { top.typeQualifiers = t.typeQualifiers;
      top.mutateTypeSpecifiers = t.mutateTypeSpecifiers;
      top.specialSpecifiers = h.specialSpecifiers ++ t.specialSpecifiers; }
| h::Attributes_c
    { top.typeQualifiers = [];
      top.mutateTypeSpecifiers = [];
      top.specialSpecifiers = h.specialSpecifiers; }

-- GCC must lie. This is ambiguous with the following syntax.
--concrete productions top::StructDeclaration_c
--| sqs::SpecifierQualifierList_c sds::StructDeclaratorList_c aa::Attributes_c ';'
--    {}

concrete productions top::StructDeclarator_c
| d::Declarator_c  aa::Attributes_c
    { top.ast =
        [ast:warnStructField([wrn(aa.location, "Ignoring attributes")]),
        ast:structField(d.declaredIdent, d.ast)]; }
| d::Declarator_c ':' e::ConstantExpr_c  aa::Attributes_c
    { top.ast = 
        [ast:warnStructField([wrn(aa.location, "Ignoring attributes")]),
        ast:structBitfield(ast:justName(d.declaredIdent), d.ast, e.ast)]; }
| ':' e::ConstantExpr_c  aa::Attributes_c
    { top.ast = 
        [ast:warnStructField([wrn(aa.location, "Ignoring attributes")]),
        ast:structBitfield(ast:nothingName(), top.givenType, e.ast)]; }


concrete productions top::StructOrUnionSpecifier_c
| su::StructOrUnion_c aa::Attributes_c id::Identifier_t '{' ss::StructDeclarationList_c '}'
    { top.realTypeSpecifiers =
        [ast:warnTypeExpr([wrn(aa.location, "Ignoring attributes")], 
          case su of
          | struct_c(_) -> ast:structTypeExpr(top.givenQualifiers, ast:structDecl(ast:justName(ast:fromId(id)), ast:foldStructItem(ss.ast)))
          | union_c(_) -> ast:unionTypeExpr(top.givenQualifiers, ast:unionDecl(ast:justName(ast:fromId(id)), ast:foldStructItem(ss.ast)))
          end)];
    }
| su::StructOrUnion_c id::Identifier_t '{' '}'
    { top.realTypeSpecifiers =
        case su of
        | struct_c(_) -> [ast:structTypeExpr(top.givenQualifiers, ast:structDecl(ast:justName(ast:fromId(id)), ast:foldStructItem([])))]
        | union_c(_) -> [ast:unionTypeExpr(top.givenQualifiers, ast:unionDecl(ast:justName(ast:fromId(id)), ast:foldStructItem([])))]
        end; }
| su::StructOrUnion_c '{' '}'
    { top.realTypeSpecifiers =
        case su of
        | struct_c(_) -> [ast:structTypeExpr(top.givenQualifiers, ast:structDecl(ast:nothingName(), ast:foldStructItem([])))]
        | union_c(_) -> [ast:unionTypeExpr(top.givenQualifiers, ast:unionDecl(ast:nothingName(), ast:foldStructItem([])))]
        end; }

------------------
-- Parsing conflict.  We could be at:

--  abstract-declarator:  '(' . ParameterTypeList ')'
--    direct-declarator:  '(' . attributes declarator ')'

-- Normally, these two cases are distinguished by the presence of DeclarationSpecifiers,
-- but of course, we've added attributes to that.

-- TODO: we NEED a solution to this syntax problem, because it will cause incorrect
-- recognition as an abstract rather than direct declarator
-- and that might lead to incorrect parse.

--concrete productions top::DirectDeclarator_c
--| '(' Attributes_c d::Declarator_c ')'
--    { top.declaredIdents = d.declaredIdents;
--      top.declaredParamIdents = d.declaredParamIdents;
--    }

-- Here, we might be at:

-- abstract-declarator:  '(' . ParameterTypeList ')'
-- abstract-declarator:  '(' . Attributes AbstractDeclarator ')'

-- Again, normally distinguished by presence of DeclarationSpecifiers.

--concrete productions top::DirectAbstractDeclarator_c
--| '(' Attributes_c d::AbstractDeclarator_c ')'
--    {}

-- If we forced a shift, we'd need this production to repair the parse.
--| '(' Attributes_c ptl::ParameterTypeList_c ')'
--    {}
------------------------


-- TODO: conflict with attributes in typequalifierlist above? what context?
--concrete productions top::ParameterDeclaration_c
--| dspecs::DeclarationSpecifiers_c dcl::Declarator_c Attributes_c
--    { top.declaredIdents = dcl.declaredIdents; }
--| dspecs::DeclarationSpecifiers_c adcl::AbstractDeclarator_c Attributes_c
--    { top.declaredIdents = []; }
-- wtf gcc, first declspecs have attributes and then they're supposed to appear here??
--| dspecs::DeclarationSpecifiers_c Attributes_c
--    { top.declaredIdents = []; }

