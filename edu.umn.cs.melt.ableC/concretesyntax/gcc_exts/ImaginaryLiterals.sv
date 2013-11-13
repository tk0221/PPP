grammar edu:umn:cs:melt:ableC:concretesyntax:gcc_exts;

-- differs only in the inclusion of IiJj and allows no dot
terminal ImaginaryFloatConstant_t   /
	(([0-9]+[\.][0-9]*)|
	 ([0-9]*[\.][0-9]+)|
	 ([0-9]+))
	([Ee][\+\-]?[0-9]+)?
	[FLfl]*[IiJj][FLfl]*
	/ lexer classes {Cliteral}, submits to {FloatConstant_t};
terminal ImaginaryHexFloatConst_t   /
	0[xX]
	(([a-fA-F0-9]+[\.][a-fA-F0-9]*)|
	 ([a-fA-F0-9]*[\.][a-fA-F0-9]+)|
	 ([a-fA-F0-9]+))
	([Pp][\+\-]?[0-9]+)?
	[FLfl]*[IiJj][FLfl]*
	/ lexer classes {Cliteral};


concrete productions top::PrimaryExpr_c
| l::ImaginaryFloatConstant_t
    { top.ast = ast:imaginaryLiteral(l.lexeme, location=top.location); }
| l::ImaginaryHexFloatConst_t
    { top.ast = ast:imaginaryHexLiteral(l.lexeme, location=top.location); }


terminal GNU_UUReal_t '__real' lexer classes {Ckeyword};
terminal GNU_UURealUU_t '__real__' lexer classes {Ckeyword};
terminal GNU_UUImag_t '__imag' lexer classes {Ckeyword};
terminal GNU_UUImagUU_t '__imag__' lexer classes {Ckeyword};

concrete productions top::UnaryOp_c
| '__real'  { top.ast = ast:realOp(location=top.location); }
| '__real__'  { top.ast = ast:realOp(location=top.location); }
| '__imag'  { top.ast = ast:imagOp(location=top.location); }
| '__imag__'  { top.ast = ast:imagOp(location=top.location); }

