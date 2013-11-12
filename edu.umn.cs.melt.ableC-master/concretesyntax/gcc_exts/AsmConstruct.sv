grammar edu:umn:cs:melt:ableC:concretesyntax:gcc_exts;

terminal Asm_t 'asm' lexer classes {Ckeyword};
terminal CPP_Asm_t '__asm__' lexer classes {Ckeyword};
terminal CPP_UUAsm_t '__asm' lexer classes {Ckeyword};

-- Syntax used external to this file:

closed nonterminal SimpleAsmStatement_c with location;
concrete productions top::SimpleAsmStatement_c
| Asm_Starter_c '(' StringConstant_c ')'
    {}

closed nonterminal Asm_Statement_c with location;
concrete productions top::Asm_Statement_c
| Asm_Starter_c ds::TypeQualifier_c '(' AsmArgument_c ')' ';'
    {}
| Asm_Starter_c '(' AsmArgument_c ')' ';'
    {}

-- Details:

closed nonterminal Asm_Starter_c with location;
concrete productions top::Asm_Starter_c
| 'asm'
    {}
| '__asm__'
    {}
| '__asm'
    {}

closed nonterminal AsmArgument_c with location;
concrete productions top::AsmArgument_c
| StringLiteral_t
    {}
| StringLiteral_t ':' AsmOperands_c
    {}
| StringLiteral_t ':' 
    {}
| StringLiteral_t ':' AsmOperands_c ':' AsmOperands_c
    {}
| StringLiteral_t ':'  ':' AsmOperands_c
    {}
| StringLiteral_t ':' AsmOperands_c ':' 
    {}
| StringLiteral_t ':'  ':' 
    {}
| StringLiteral_t ':' AsmOperands_c ':' AsmOperands_c ':' AsmClobbers_c
    {}
| StringLiteral_t ':'  ':' AsmOperands_c ':' AsmClobbers_c
    {}
| StringLiteral_t ':' AsmOperands_c ':'  ':' AsmClobbers_c
    {}
| StringLiteral_t ':'  ':'  ':' AsmClobbers_c
    {}

closed nonterminal AsmClobbers_c with location;
concrete productions top::AsmClobbers_c
| StringLiteral_t
    {}
| AsmClobbers_c ',' StringLiteral_t
    {}

closed nonterminal AsmOperands_c with location;
concrete productions top::AsmOperands_c
| AsmOperand_c
    {}
| AsmOperands_c ',' AsmOperand_c
    {}

closed nonterminal AsmOperand_c with location;
concrete productions top::AsmOperand_c
| StringLiteral_t '(' Expr_c ')'
    {}
| '[' Identifier_t ']' StringLiteral_t '(' Expr_c ')'
    {}

