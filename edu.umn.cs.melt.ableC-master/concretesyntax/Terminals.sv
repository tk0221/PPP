grammar edu:umn:cs:melt:ableC:concretesyntax;

{--
 - Largely exists to permit embedding into other languages. Allows easy
 - dominates relationship
 -}

--Andrew's Part, taken from the Simple terminals file and possibly modified 
temp_imp_ide_font font_all color(123, 0, 82) bold;
temp_imp_ide_font font_comments color(63, 95, 191) italic;
temp_imp_ide_font font_special_symbol color(71, 71, 141);
temp_imp_ide_font font_equal color(63, 127, 95);


lexer class Ccomment font = font_comments;
lexer class Ckeyword font = font_all;
lexer class Cassignment font = font_equal;
lexer class Csymbol font = font_special_symbol;


--


ignore terminal LineComment
  /[\/][\/].*/ 
  lexer classes {Ccomment};

ignore terminal BlockComment 
  /[\/][\*]([^\*]|[\r\n]|([\*]+([^\*\/]|[\r\n])))*[\*]+[\/]/ 
  lexer classes {Ccomment};

ignore terminal WhiteSpace
  /[\n\t\ ]+/ 
  lexer classes {Ccomment};

{--
 - Identifiers: normal or type name.
 -}
lexer class Cidentifier submits to Ckeyword;

terminal Identifier_t /[A-Za-z_\$][A-Za-z_0-9\$]*/ lexer classes {Cidentifier};
terminal TypeName_t   /[A-Za-z_\$][A-Za-z_0-9\$]*/ lexer classes {Cidentifier};
-- See LexerHack.sv for code related to disambiguation of these terminals.


{--
 - Literals
 -}
lexer class Cliteral;

-- TODO: Expand these to proper nonterminals parsing the expressions!
-- TODO: Also, missing octal here...
terminal DecimalConstant_t /[0-9]+[uUlL]*/ lexer classes {Cliteral};
terminal Constant_t        /0[xX][a-fA-F0-9]*[uUlL]*/ lexer classes {Cliteral};
terminal FloatConstant_t   /
	(([0-9]+[\.][0-9]*)|
	 ([0-9]*[\.][0-9]+))
	([Ee][\+\-]?[0-9]+)?
	[FLfl]*
	/ lexer classes {Cliteral};
terminal HexFloatConst_t   /
	0[xX]
	(([a-fA-F0-9]+[\.][a-fA-F0-9]*)|
	 ([a-fA-F0-9]*[\.][a-fA-F0-9]+))
	([Pp][\+\-]?[0-9]+)?
	[FLfl]*
	/ lexer classes {Cliteral};
terminal StringLiteral_t   /[Uu8L]*[\"]([^\"]|[\\][\"])*[\"]/ lexer classes {Cliteral};
terminal CharLiteral_t     /[Uu8L]*[\']([^\']|[\\].)[\']/ lexer classes {Cliteral};


{--
 - Keywords
 -}


-- types
terminal Char_t     'char'     lexer classes {Ckeyword};
terminal Double_t   'double'   lexer classes {Ckeyword};
terminal Float_t    'float'    lexer classes {Ckeyword};
terminal Int_t      'int'      lexer classes {Ckeyword};
terminal Long_t     'long'     lexer classes {Ckeyword};
terminal Short_t    'short'    lexer classes {Ckeyword};
terminal Signed_t   'signed'   lexer classes {Ckeyword};
terminal Unsigned_t 'unsigned' lexer classes {Ckeyword};
terminal Void_t     'void'     lexer classes {Ckeyword};
terminal Bool_t     '_Bool'      lexer classes {Ckeyword}; -- c99
terminal Complex_t  '_Complex'   lexer classes {Ckeyword}; -- c99
terminal Imagin_t   '_Imaginary' lexer classes {Ckeyword}; -- c99

-- Er, Specifiers?
terminal ENUM   'enum'   lexer classes {Ckeyword};
terminal STRUCT 'struct' lexer classes {Ckeyword};
terminal UNION  'union'  lexer classes {Ckeyword};

-- Qualifiers
terminal Const_t    'const'    lexer classes {Ckeyword};
terminal Volatile_t 'volatile' lexer classes {Ckeyword};
terminal Restrict_t 'restrict' lexer classes {Ckeyword}; -- c99

-- Function specifiers
terminal Inline_t   'inline'   lexer classes {Ckeyword}; -- c99

-- Storage class specifiers
terminal Auto_t     'auto'     lexer classes {Ckeyword};
terminal Extern_t   'extern'   lexer classes {Ckeyword};
terminal Register_t 'register' lexer classes {Ckeyword};
terminal Static_t   'static'   lexer classes {Ckeyword};
terminal Typedef_t  'typedef'  lexer classes {Ckeyword};

-- Statement keywords
terminal BREAK    'break'    lexer classes {Ckeyword};
terminal CASE     'case'     lexer classes {Ckeyword};
terminal CONTINUE 'continue' lexer classes {Ckeyword};
terminal DEFAULT  'default'  lexer classes {Ckeyword};
terminal DO       'do'       lexer classes {Ckeyword};
terminal ELSE     'else'     lexer classes {Ckeyword}, precedence = 2, association = left;
terminal FOR      'for'      lexer classes {Ckeyword};
terminal GOTO     'goto'     lexer classes {Ckeyword};
terminal IF       'if'       lexer classes {Ckeyword};
terminal Return_t 'return'   lexer classes {Ckeyword};
terminal SWITCH   'switch'   lexer classes {Ckeyword};
terminal WHILE    'while'    lexer classes {Ckeyword};

-- Expression keywords
terminal SIZEOF   'sizeof'   lexer classes {Ckeyword};

-- Structural symbols
terminal Comma_t       ',';
terminal Semi_t        ';';
terminal LParen_t      '(';
terminal RParen_t      ')' precedence = 1, association = left; -- evidently, part of dangling-else?
terminal LBracket_t    '[';
terminal RBracket_t    ']';
terminal LCurly_t      '{'  action { context = head(context) :: context; };
terminal RCurly_t      '}'  action { context = tail(context); };

terminal Question_t    '?';
terminal Colon_t       ':';

-- Dereference operators
terminal Dot_t         '.';
terminal PTR_OP        '->';

-- Assignment operators
terminal Assign_t      '='    lexer classes {Cassignment};
terminal RIGHT_ASSIGN  '>>='    lexer classes {Cassignment}; 
terminal LEFT_ASSIGN   '<<='    lexer classes {Cassignment};
terminal ADD_ASSIGN    '+='    lexer classes {Cassignment};
terminal SUB_ASSIGN    '-='    lexer classes {Cassignment};
terminal MUL_ASSIGN    '*='    lexer classes {Cassignment};
terminal DIV_ASSIGN    '/='    lexer classes {Cassignment};
terminal MOD_ASSIGN    '%='    lexer classes {Cassignment};
terminal AND_ASSIGN    '&='    lexer classes {Cassignment};
terminal XOR_ASSIGN    '^='    lexer classes {Cassignment};
terminal OR_ASSIGN     '|='    lexer classes {Cassignment};

-- Bit operators
terminal And_t         '&'    lexer classes {Csymbol}; -- address of
terminal Or_t          '|'    lexer classes {Csymbol};
terminal Tilde_t       '~'    lexer classes {Csymbol};
terminal Xor_t         '^'    lexer classes {Csymbol};
terminal RIGHT_OP      '>>'    lexer classes {Csymbol};
terminal LEFT_OP       '<<'    lexer classes {Csymbol};

-- Numerical operators
terminal Minus_t       '-'  precedence = 5, association = left, lexer classes {Csymbol}; -- negative
terminal Plus_t        '+'  precedence = 5, association = left,  lexer classes {Csymbol}; -- positive
terminal Star_t        '*'  precedence = 6, association = left, lexer classes {Csymbol}; -- pointer, deref
terminal Divide_t      '/'  precedence = 6, association = left, lexer classes {Csymbol};
terminal Mod_t         '%';

-- Logical operators
terminal Not_t   '!';
terminal AndOp_t '&&' precedence = 4, association = left, lexer classes {Csymbol};
terminal OrOp_t  '||' precedence = 4, association = left, lexer classes {Csymbol};

-- Comparison operators
terminal LessThan_t         '<'  precedence = 3, association = left, lexer classes {Csymbol};
terminal GreaterThan_t      '>'  precedence = 3, association = left, lexer classes {Csymbol};
terminal LessThanEqual_t    '<=' precedence = 3, association = left, lexer classes {Csymbol};
terminal GreaterThanEqual_t '>=' precedence = 3, association = left, lexer classes {Csymbol};
terminal Equality_t         '==' precedence = 3, association = left, lexer classes {Csymbol};
terminal NonEquality_t      '!=' precedence = 3, association = left, lexer classes {Csymbol};

-- *crement operators
terminal INC_OP        '++'    lexer classes {Csymbol};
terminal DEC_OP        '--'    lexer classes {Csymbol};

-- Varargs syntax
terminal ELLIPSES      '...'    lexer classes {Csymbol};

-- High precedence empty terminal, for some reason?
terminal Cpp_Attribute_high_prec // precedence = 20;

