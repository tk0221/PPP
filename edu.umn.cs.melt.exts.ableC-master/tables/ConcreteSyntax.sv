grammar edu:umn:cs:melt:exts:ableC:tables ;

marking terminal Table_t 'table' lexer classes {Ckeyword} ;
--terminal Table_t 'table' dominates { Identifier_t, TypeName_t }, prefix = true;

--Sample Table
--
--     c4 = table (
--         c1 : T F,
--         c2 : F *,
--         c3 : T T );


concrete production table_c
t::cnc:PrimaryExpr_c ::=  'table' '(' trows::TableRows_c ')' 
{ t.ast = table (trows.ast, location=t.location); 
}


-- Table Rows --
----------------
nonterminal TableRows_c with ast<TableRows>, location ;
nonterminal TableRow_c  with ast<TableRow>, location ;

concrete production tableRowSnoc_c
trows::TableRows_c ::= trowstail::TableRows_c ',' trow::TableRow_c 
{ trows.ast = tableRowSnoc (trowstail.ast, trow.ast, location=trows.location); 
}

concrete production tableRowOne_c
trows::TableRows_c ::= trow::TableRow_c
{ trows.ast = tableRowOne (trow.ast, location=trows.location); 
}

concrete production tableRow_c
trow::TableRow_c ::= e::cnc:Expr_c ':' tvs::TruthValueList_c
{ trow.ast = tableRow(e.ast, tvs.ast, location=trow.location); 
}


--Truth Value List
-----------------------
nonterminal TruthValueList_c with ast<TruthFlagList>, location; 

concrete production tvlistCons_c
tvl::TruthValueList_c ::=  tv::TruthValue_c  tvltail::TruthValueList_c 
{ tvl.ast = tvlistCons(tv.ast, tvltail.ast); 
}

concrete production tvlistOne_c
tvl::TruthValueList_c ::=  tv::TruthValue_c 
{ tvl.ast = tvlistOne( tv.ast );
}


-- Truth Values
---------------
terminal TrueTV_t   'T'  ; 
terminal FalseTV_t  'F'  ; 
terminal StarTV_t   '*'  ; 

nonterminal TruthValue_c with ast<TruthFlag>, location ; 

concrete production tvTrue_c
tv::TruthValue_c ::= truetv::TrueTV_t 
{ tv.ast = tvTrue() ; }

concrete production tvFalse_c
tv::TruthValue_c ::= falsetv::FalseTV_t 
{ tv.ast = tvFalse() ; }

concrete production tvStar_c
tv::TruthValue_c ::= startv::StarTV_t
{ tv.ast = tvStar() ; }
