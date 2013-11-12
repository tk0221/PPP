grammar edu:umn:cs:melt:ableC:concretesyntax:gcc_exts;

-- We have AttrList in Lists of other stuff, this precedence is here to each it up in one go
terminal CPP_Attribute_t '__attribute__' lexer classes {Ckeyword}, precedence=10;
terminal CPP_UUAttribute_t '__attribute' lexer classes {Ckeyword}, precedence=10;
terminal CPP_Attr_LowerPrec_t // precedence=5;

closed nonterminal Attributes_c with location, specialSpecifiers;
concrete productions top::Attributes_c
| h::Attribute_c
    operator=CPP_Attr_LowerPrec_t -- shift/reduce on Attr? shift! gobble up the whole list.
    { top.specialSpecifiers = [ast:attributeQualifier()]; }
| h::Attribute_c t::Attributes_c
    { top.specialSpecifiers = ast:attributeQualifier() :: t.specialSpecifiers; }

closed nonterminal Attribute_c with location;
concrete productions top::Attribute_c
| '__attribute__' '(' '(' a::AttributeList_c ')' ')'
    {}
| '__attribute' '(' '(' a::AttributeList_c ')' ')'
    {}

closed nonterminal AttributeList_c with location;
concrete productions top::AttributeList_c
| Attrib_c
    {}
| AttributeList_c ',' Attrib_c
    {}

closed nonterminal Attrib_c with location;
concrete productions top::Attrib_c
| 
    {}
| AttribName_c
    {}
-- TODO: This is supposed to be separately recognized syntax, probably with a specific meaning.
-- However, it's in conflict with the Identifier just being recognized as an expression.
-- We omit it from the syntax, but perhaps we should specially recognize this case
-- in the translation of the next case to abstract syntax.
--| AttribName_c '(' Identifier_t ')'
 --   {}
--| AttribName_c '(' Identifier_t ',' ArgumentExprList_c ')'
--    {}
| AttribName_c '(' ArgumentExprList_c ')'
    {}

closed nonterminal AttribName_c with location;
concrete productions top::AttribName_c
| Identifier_t
    {}
| TypeSpecifier_c
    {}
| TypeQualifier_c
    {}
| StorageClassSpecifier_c
    {}


