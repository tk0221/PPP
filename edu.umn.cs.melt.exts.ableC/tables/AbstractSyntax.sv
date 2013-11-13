grammar edu:umn:cs:melt:exts:ableC:tables ;

imports edu:umn:cs:melt:ableC:concretesyntax as cnc ;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs ;
imports edu:umn:cs:melt:ableC:abstractsyntax:env ;

imports silver:langutil only ast, pp, errors, err ;
imports silver:langutil:pp ;

--  In
--      c = table (
--          e1 : T F,
--          e2 : F *,
--          e3 : T T );
-- 
-- table forwards to
--   
--   ({ int c1 = e1; int c2 = e2; int c3 = e3;
--      (c1 && ! c2 && c3) || ( ! c1 && 1 && c3 ) ; })


-- Abstract Syntax --

abstract production table
t::abs:Expr ::= trows::TableRows 
{
 -- t.pp = concat( [text("table ("), line(), trows.pp, text(" )")] ) ;

  t.errors := trows.errors ;

 forwards to result_expr ;

 local attribute result_expr :: abs:Expr ;
 result_expr = --if !null(trows.errors)
               --then conlit ( terminal(DecimalConstant_t,"1") ) 
               --else 
               disjunction(mapConjunction( transpose
                         ( trows.ftExprss ))) ;

 local attribute foo :: abs:Expr 
  = head ( head ( trows.ftExprss ) ) ;
}

-- Table Rows --
----------------
nonterminal TableRows with
  pp, errors, ftExprss, rlen, location, env ; 

synthesized attribute ftExprss :: [[abs:Expr]] ;
synthesized attribute rlen :: Integer ;

abstract production tableRowSnoc
trows::TableRows ::=  trowstail::TableRows trow::TableRow
{
 trows.pp = concat( [trowstail.pp, line(), trow.pp] );

 trows.errors := trowstail.errors ++ trow.errors ;
 trows.errors <-
   if   trow.rlen == trowstail.rlen 
   then [ ]
   else [ err ( trowstail.location,
                "The number of T,F,* entries in table row must be the same " ++ 
                "as the preceding rows") ] ;

 trows.rlen = trow.rlen ;
 trows.ftExprss = trowstail.ftExprss ++ [trow.ftExprs] ;
}

abstract production tableRowOne
trows::TableRows ::= trow::TableRow
{
 trows.pp = trow.pp ;
 trows.errors := trow.errors ;
 trows.rlen = trow.rlen ; 
 trows.ftExprss = [ trow.ftExprs ] ;
}

-- Table Row --
---------------
nonterminal TableRow with 
  pp, errors, ftExprs, rlen, location, env ;

synthesized attribute ftExprs :: [ abs:Expr ] ;

abstract production tableRow
trow::TableRow ::= e::abs:Expr tvl::TruthFlagList
{
 trow.pp =  concat( [e.pp, text(" : "), tvl.pp] );
 trow.errors := e.errors ;
 trow.rlen = tvl.rlen ;

 trow.ftExprs = tvl.ftExprs ;
 tvl.rowExpr = e ; 
}

-- Truth Value List
-------------------
nonterminal TruthFlagList with
  pp, rowExpr, ftExprs, rlen ;

inherited attribute rowExpr :: abs:Expr ;
-- the expression in the table row.  It is passed down to the TF*
-- values to be used in the translation to the host language.

abstract production tvlistCons
tvl::TruthFlagList ::=  tv::TruthFlag  tvltail::TruthFlagList
{
 tvl.pp = concat( [tv.pp, text(" "), tvltail.pp] );
 tvl.rlen = 1 + tvltail.rlen ;
 tvl.ftExprs = cons (tv.ftExpr, tvltail.ftExprs ) ;

 tv.rowExpr = tvl.rowExpr ;
 tvltail.rowExpr = tvl.rowExpr ;
}

abstract production tvlistOne
tvl::TruthFlagList ::=  tv::TruthFlag
{
 tvl.pp = tv.pp ;
 tvl.rlen = 1 ;
 tvl.ftExprs = [ tv.ftExpr ] ;
 tv.rowExpr = tvl.rowExpr ;
 
}


-- Truth Values
---------------
nonterminal TruthFlag with 
  pp, rowExpr, ftExpr;

synthesized attribute ftExpr :: abs:Expr ;

abstract production tvTrue
tv::TruthFlag ::=
{
 tv.pp = text("T") ;
 tv.ftExpr = tv.rowExpr ;
}

abstract production tvFalse
tv::TruthFlag ::=
{
 tv.pp = text("F") ;
 tv.ftExpr = logicalNegate ( tv.rowExpr ) ;
}

abstract production tvStar
tv::TruthFlag ::=
{ 
 tv.pp = text("*") ;
 tv.ftExpr = abs:integerLiteral("1", location=tv.rowExpr.location);
}


-- helper expressions --
------------------------
function logicalNegate
abs:Expr ::= ne::abs:Expr
{ return --forwards to
    abs:unaryOpExpr( 
      abs:notOp(location=ne.location), 
       ne,
       location=ne.location );
}

function logicalOr
abs:Expr ::= e1::abs:Expr e2::abs:Expr
{ return abs:binaryOpExpr (e1, op, e2, location=e1.location) ;
  local attribute op :: abs:BinOp 
    = abs:boolOp( abs:orBoolOp(location=e1.location), location=e1.location) ;
}

function logicalAnd
abs:Expr ::= e1::abs:Expr e2::abs:Expr
{ return abs:binaryOpExpr (e1, op, e2, location=e1.location) ;
  local attribute op :: abs:BinOp 
    = abs:boolOp( abs:andBoolOp(location=e1.location), location=e1.location) ;
}

-- table helper functions
-------------------------
function disjunction abs:Expr ::= es::[abs:Expr]
{ return if length(es) == 1  then head(es)
         else logicalOr ( head(es), disjunction ( tail(es) ) ) ;
}

function mapConjunction [abs:Expr] ::= ess::[[abs:Expr]]
{ return if null(ess)  then [ ] 
         else cons ( conjunction ( head(ess) ),
                     mapConjunction ( tail(ess) ) ) ;
}

function conjunction abs:Expr ::= es::[abs:Expr]
{ return if length(es) == 1    then head(es)
         else logicalAnd ( head(es), conjunction ( tail(es) ) ) ;
}

function transpose
[[a]] ::= m::[[a]]
{ return
    case m of
    | [ ]::_ -> [ ]
    | _      -> map(head,m) :: transpose ( map(tail,m) ) 
    end ;

}

