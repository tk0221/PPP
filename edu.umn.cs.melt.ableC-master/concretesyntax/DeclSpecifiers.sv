grammar edu:umn:cs:melt:ableC:concretesyntax;

-- This file is separate from Declarations.sv because there are only a few
-- symbols it "exports" which hide quite a lot of syntax beneath them,
-- but only ever get referenced through the few "exported" symbols.

{--
 - Identifies whether 'typedef' appeared in the declaration specifiers.
 - We keep this separate from the other storage classes, as it's not really
 - a storage class at all, and the abstract syntax treats it differently.
 -}
synthesized attribute isTypedef :: Boolean;
{--
 - Identifies the storage class of this set of declaration specifiers.
 -}
synthesized attribute storageClass :: [ast:StorageClass];
{--
 - A list of not-yet interpreted (hence, Pre-) type specifiers
 - e.g. ["long", "long"]
 - This is mutually exclusive with realTypeSpecifiers.
 - @see interpretTypeSpecifiers in AstContructionHelpers.sv
 -}
synthesized attribute preTypeSpecifiers :: [String];
{--
 - A list of "real" type specifiers, e.g. "struct Foo"
 - where only exactly one specifier is permitted.
 - This is mutually exclusive with preTypeSpecifiers.
 -}
synthesized attribute realTypeSpecifiers :: [ast:BaseTypeExpr];
{--
 - Mutators for type specifiers, there should only ever be 0 or 1.
 -}
synthesized attribute mutateTypeSpecifiers :: [ast:TypeSpecifierMutator];
{--
 - A list of type qualifiers.
 -}
synthesized attribute typeQualifiers :: [ast:Qualifier];
{--
 - A list of special specifiers (e.g. inline, noreturn, alignas)
 -}
synthesized attribute specialSpecifiers :: [ast:SpecialSpecifier];
{--
 - A list of the qualifiers attached to this declaration, somehow.
 -}
autocopy attribute givenQualifiers :: [ast:Qualifier];

-- "Exported" symbols. These are used elsewhere in the C grammar.


closed nonterminal DeclarationSpecifiers_c with location, isTypedef, storageClass, preTypeSpecifiers, realTypeSpecifiers, typeQualifiers, specialSpecifiers, givenQualifiers, mutateTypeSpecifiers;
concrete productions top::DeclarationSpecifiers_c
| h::StorageClassSpecifier_c  t::DeclarationSpecifiers_c
    { top.isTypedef = h.isTypedef || t.isTypedef;
      top.storageClass = h.storageClass ++ t.storageClass;
      top.preTypeSpecifiers = t.preTypeSpecifiers;
      top.realTypeSpecifiers = t.realTypeSpecifiers;
      top.typeQualifiers = t.typeQualifiers;
      top.specialSpecifiers = t.specialSpecifiers;
      top.mutateTypeSpecifiers = t.mutateTypeSpecifiers; }
| h::StorageClassSpecifier_c
    { top.isTypedef = h.isTypedef;
      top.storageClass = h.storageClass;
      top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [];
      top.typeQualifiers = [];
      top.specialSpecifiers = [];
      top.mutateTypeSpecifiers = []; }
| h::TypeSpecifier_c  t::DeclarationSpecifiers_c
    { top.isTypedef = t.isTypedef;
      top.storageClass = t.storageClass;
      top.preTypeSpecifiers = h.preTypeSpecifiers ++ t.preTypeSpecifiers;
      top.realTypeSpecifiers = h.realTypeSpecifiers ++ t.realTypeSpecifiers;
      top.typeQualifiers = t.typeQualifiers;
      top.specialSpecifiers = t.specialSpecifiers;
      top.mutateTypeSpecifiers = t.mutateTypeSpecifiers; }
| h::TypeSpecifier_c
    { top.isTypedef = false;
      top.storageClass = [];
      top.preTypeSpecifiers = h.preTypeSpecifiers;
      top.realTypeSpecifiers = h.realTypeSpecifiers;
      top.typeQualifiers = [];
      top.specialSpecifiers = [];
      top.mutateTypeSpecifiers = []; }
| h::TypeQualifier_c  t::DeclarationSpecifiers_c
    { top.isTypedef = t.isTypedef;
      top.storageClass = t.storageClass;
      top.preTypeSpecifiers = t.preTypeSpecifiers;
      top.realTypeSpecifiers = t.realTypeSpecifiers;
      top.typeQualifiers = h.typeQualifiers ++ t.typeQualifiers;
      top.specialSpecifiers = t.specialSpecifiers;
      top.mutateTypeSpecifiers = h.mutateTypeSpecifiers ++ t.mutateTypeSpecifiers; }
| h::TypeQualifier_c
    { top.isTypedef = false;
      top.storageClass = [];
      top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [];
      top.typeQualifiers = h.typeQualifiers;
      top.specialSpecifiers = [];
      top.mutateTypeSpecifiers = h.mutateTypeSpecifiers; }
| h::FunctionSpecifier_c  t::DeclarationSpecifiers_c
    { top.isTypedef = t.isTypedef;
      top.storageClass = t.storageClass;
      top.preTypeSpecifiers = t.preTypeSpecifiers;
      top.realTypeSpecifiers = t.realTypeSpecifiers;
      top.typeQualifiers = t.typeQualifiers; 
      top.specialSpecifiers = h.specialSpecifiers ++ t.specialSpecifiers;
      top.mutateTypeSpecifiers = t.mutateTypeSpecifiers; }
| h::FunctionSpecifier_c
    { top.isTypedef = false;
      top.storageClass = [];
      top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [];
      top.typeQualifiers = [];
      top.specialSpecifiers = h.specialSpecifiers;
      top.mutateTypeSpecifiers = []; }


closed nonterminal SpecifierQualifierList_c with location, preTypeSpecifiers, realTypeSpecifiers, typeQualifiers, givenQualifiers, mutateTypeSpecifiers, specialSpecifiers;
concrete productions top::SpecifierQualifierList_c
| h::TypeSpecifier_c  t::SpecifierQualifierList_c
    { top.preTypeSpecifiers = h.preTypeSpecifiers ++ t.preTypeSpecifiers;
      top.realTypeSpecifiers = h.realTypeSpecifiers ++ t.realTypeSpecifiers;
      top.typeQualifiers = t.typeQualifiers;
      top.mutateTypeSpecifiers = t.mutateTypeSpecifiers;
      top.specialSpecifiers = t.specialSpecifiers; }
| h::TypeSpecifier_c 
    { top.preTypeSpecifiers = h.preTypeSpecifiers;
      top.realTypeSpecifiers = h.realTypeSpecifiers;
      top.typeQualifiers = [];
      top.mutateTypeSpecifiers = [];
      top.specialSpecifiers = []; }
| h::TypeQualifier_c  t::SpecifierQualifierList_c
    { top.preTypeSpecifiers = t.preTypeSpecifiers;
      top.realTypeSpecifiers = t.realTypeSpecifiers;
      top.typeQualifiers = h.typeQualifiers ++ t.typeQualifiers;
      top.mutateTypeSpecifiers = h.mutateTypeSpecifiers ++ t.mutateTypeSpecifiers;
      top.specialSpecifiers = t.specialSpecifiers; }
| h::TypeQualifier_c 
    { top.preTypeSpecifiers = [];
      top.realTypeSpecifiers = [];
      top.typeQualifiers = h.typeQualifiers;
      top.mutateTypeSpecifiers = h.mutateTypeSpecifiers;
      top.specialSpecifiers = []; }

closed nonterminal TypeQualifierList_c with location, typeQualifiers, mutateTypeSpecifiers, specialSpecifiers;
concrete productions top::TypeQualifierList_c
| h::TypeQualifier_c
    { top.typeQualifiers = h.typeQualifiers;
      top.mutateTypeSpecifiers = h.mutateTypeSpecifiers;
      top.specialSpecifiers = []; }
| h::TypeQualifier_c  t::TypeQualifierList_c
    { top.typeQualifiers = h.typeQualifiers ++ t.typeQualifiers;
      top.mutateTypeSpecifiers = h.mutateTypeSpecifiers ++ t.mutateTypeSpecifiers;
      top.specialSpecifiers = t.specialSpecifiers; }


-- "Non-exported" symbols. These are only used directly in this file.

closed nonterminal StorageClassSpecifier_c with location, isTypedef, storageClass; 
concrete productions top::StorageClassSpecifier_c
| 'typedef'
    { top.isTypedef = true;
      top.storageClass = []; }
| 'extern'
    { top.isTypedef = false;
      top.storageClass = [ast:externStorageClass()]; }
| 'static'
    { top.isTypedef = false;
      top.storageClass = [ast:staticStorageClass()]; }
| 'auto'
    { top.isTypedef = false;
      top.storageClass = [ast:autoStorageClass()]; }
| 'register'
    { top.isTypedef = false;
      top.storageClass = [ast:registerStorageClass()]; }


closed nonterminal TypeSpecifier_c with location, preTypeSpecifiers, realTypeSpecifiers, givenQualifiers; 
concrete productions top::TypeSpecifier_c
| 'void'
    { top.realTypeSpecifiers = [ast:directTypeExpr(ast:builtinType(top.givenQualifiers, ast:voidType()))];
      top.preTypeSpecifiers = []; }
| 'char'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["char"]; }
| 'short'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["short"]; }
| 'int'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["int"]; }
| 'long'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["long"]; }
| 'float'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["float"]; }
| 'double'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["double"]; }
| 'signed'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["signed"]; }
| 'unsigned'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["unsigned"]; }
| '_Bool'
    { top.realTypeSpecifiers = [ast:directTypeExpr(ast:builtinType(top.givenQualifiers, ast:boolType()))];
      top.preTypeSpecifiers = []; }
| '_Imaginary'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["_Imaginary"]; }
| '_Complex'
    { top.realTypeSpecifiers = [];
      top.preTypeSpecifiers = ["_Complex"]; }
| s::StructOrUnionSpecifier_c 
    { top.realTypeSpecifiers = s.realTypeSpecifiers;
      top.preTypeSpecifiers = []; }
| e::EnumSpecifier_c
    { top.realTypeSpecifiers = e.realTypeSpecifiers;
      top.preTypeSpecifiers = []; }
| t::TypeName_t
    { top.realTypeSpecifiers = [ast:typedefTypeExpr(top.givenQualifiers, ast:fromTy(t))];
      top.preTypeSpecifiers = []; }


closed nonterminal TypeQualifier_c with location, typeQualifiers, mutateTypeSpecifiers; 
concrete productions top::TypeQualifier_c
| 'const'
    { top.typeQualifiers = [ast:constQualifier()];
      top.mutateTypeSpecifiers = []; }
| 'volatile'
    { top.typeQualifiers = [ast:volatileQualifier()];
      top.mutateTypeSpecifiers = []; }
| 'restrict'
    { top.typeQualifiers = [ast:restrictQualifier()];
      top.mutateTypeSpecifiers = []; }


closed nonterminal FunctionSpecifier_c with location, specialSpecifiers; 
concrete productions top::FunctionSpecifier_c
| 'inline'
    { top.specialSpecifiers = [ast:inlineQualifier()]; }


closed nonterminal StructOrUnionSpecifier_c with location, realTypeSpecifiers, givenQualifiers; 
concrete productions top::StructOrUnionSpecifier_c
| su::StructOrUnion_c id::Identifier_t '{' ss::StructDeclarationList_c '}'
    { top.realTypeSpecifiers =
        case su of
        | struct_c(_) -> [ast:structTypeExpr(top.givenQualifiers, ast:structDecl(ast:justName(ast:fromId(id)), ast:foldStructItem(ss.ast)))]
        | union_c(_) -> [ast:unionTypeExpr(top.givenQualifiers, ast:unionDecl(ast:justName(ast:fromId(id)), ast:foldStructItem(ss.ast)))]
        end; }
| su::StructOrUnion_c '{' ss::StructDeclarationList_c '}'
    { top.realTypeSpecifiers =
        case su of
        | struct_c(_) -> [ast:structTypeExpr(top.givenQualifiers, ast:structDecl(ast:nothingName(), ast:foldStructItem(ss.ast)))]
        | union_c(_) -> [ast:unionTypeExpr(top.givenQualifiers, ast:unionDecl(ast:nothingName(), ast:foldStructItem(ss.ast)))]
        end; }
| su::StructOrUnion_c id::Identifier_t
    { top.realTypeSpecifiers =
        case su of
        | struct_c(_) -> [ast:tagReferenceTypeExpr(top.givenQualifiers, ast:structSEU(), ast:fromId(id))]
        | union_c(_) -> [ast:tagReferenceTypeExpr(top.givenQualifiers, ast:unionSEU(), ast:fromId(id))]
        end; }


closed nonterminal StructOrUnion_c with location; 
concrete productions top::StructOrUnion_c
(struct_c) | 'struct'  {}
(union_c)  | 'union'  {}


closed nonterminal StructDeclarationList_c with location, ast<[ast:StructItem]>;
concrete productions top::StructDeclarationList_c
| h::StructDeclaration_c
    { top.ast = h.ast; }
| h::StructDeclarationList_c  t::StructDeclaration_c
    { top.ast = h.ast ++ t.ast; }


closed nonterminal StructDeclaration_c with location, ast<[ast:StructItem]>;
concrete productions top::StructDeclaration_c
| sqs::SpecifierQualifierList_c  decls::StructDeclaratorList_c ';'
    { top.ast = [ast:structItem(
        ast:figureOutTypeFromSpecifiers(sqs.location, sqs.typeQualifiers, sqs.preTypeSpecifiers, sqs.realTypeSpecifiers, sqs.mutateTypeSpecifiers),
        ast:foldStructDeclarator(decls.ast))];
      sqs.givenQualifiers = sqs.typeQualifiers;
      decls.givenType = ast:baseTypeExpr();
    }


closed nonterminal StructDeclaratorList_c with location, ast<[ast:StructDeclarator]>, givenType;
concrete productions top::StructDeclaratorList_c
| h::StructDeclarator_c
    { top.ast = h.ast; }
| h::StructDeclaratorList_c  ','  t::StructDeclarator_c
    { top.ast = h.ast ++ t.ast; }


closed nonterminal StructDeclarator_c with location, ast<[ast:StructDeclarator]>, givenType; 
concrete productions top::StructDeclarator_c
| d::Declarator_c
    { top.ast = [ast:structField(d.declaredIdent, d.ast)]; }
| d::Declarator_c ':' e::ConstantExpr_c
    { top.ast = [ast:structBitfield(ast:justName(d.declaredIdent), d.ast, e.ast)]; }
| ':' e::ConstantExpr_c
    { top.ast = [ast:structBitfield(ast:nothingName(), top.givenType, e.ast)]; }


closed nonterminal EnumSpecifier_c with location, realTypeSpecifiers, givenQualifiers; 
concrete productions top::EnumSpecifier_c
| 'enum' '{' en::EnumeratorList_c '}'
    { top.realTypeSpecifiers = [ast:enumTypeExpr(top.givenQualifiers, ast:enumDecl(ast:nothingName(), ast:foldEnumItem(en.ast)))]; }
| 'enum' id::Identifier_t '{' en::EnumeratorList_c '}'
    { top.realTypeSpecifiers = [ast:enumTypeExpr(top.givenQualifiers, ast:enumDecl(ast:justName(ast:fromId(id)), ast:foldEnumItem(en.ast)))]; }
| 'enum' '{' en::EnumeratorList_c ',' '}'
    { top.realTypeSpecifiers = [ast:enumTypeExpr(top.givenQualifiers, ast:enumDecl(ast:nothingName(), ast:foldEnumItem(en.ast)))]; }
| 'enum' id::Identifier_t '{' en::EnumeratorList_c ',' '}'
    { top.realTypeSpecifiers = [ast:enumTypeExpr(top.givenQualifiers, ast:enumDecl(ast:justName(ast:fromId(id)), ast:foldEnumItem(en.ast)))]; }
| 'enum' id::Identifier_t
    { top.realTypeSpecifiers = [ast:tagReferenceTypeExpr(top.givenQualifiers, ast:enumSEU(), ast:fromId(id))]; }


closed nonterminal EnumeratorList_c with location, ast<[ast:EnumItem]>;
concrete productions top::EnumeratorList_c
| h::Enumerator_c
    { top.ast = h.ast; }
| h::EnumeratorList_c  ','  t::Enumerator_c
    { top.ast = h.ast ++ t.ast; }


closed nonterminal Enumerator_c with location, ast<[ast:EnumItem]>;
concrete productions top::Enumerator_c
| id::Identifier_t
    { top.ast = [ast:enumItem(ast:fromId(id), ast:nothingExpr())]; }
| id::Identifier_t '=' ce::ConstantExpr_c
    { top.ast = [ast:enumItem(ast:fromId(id), ast:justExpr(ce.ast))]; }


