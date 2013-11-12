grammar edu:umn:cs:melt:exts:ableC:repeatUntil ;

import edu:umn:cs:melt:ableC:concretesyntax as cnc ;
import edu:umn:cs:melt:ableC:abstractsyntax as abs ;

import silver:langutil only ast ; --, errors, err, wrn;

-- Concrete Syntax --
---------------------
marking terminal Repeat_t 'repeat' lexer classes {Ckeyword} ;
terminal Until_t 'until'  lexer classes {Ckeyword} ;

concrete production repeatUntil_c
s::cnc:IterationStmt_c 
  ::= 'repeat' '{' body::cnc:BlockItemList_c '}' 
      'until'  '(' cond::cnc:Expr_c ')'
{
  s.ast = repeatUntil (
            foldr(abs:compoundStmt, abs:nullStmt(), body.ast),
            cond.ast );
}


-- Abstract Syntax --
---------------------
abstract production repeatUntil
s::abs:Stmt ::= body::abs:Stmt cond::abs:Expr
{
  forwards to 
    abs:compoundStmt ( 
      body, 
      abs:whileStmt(
        abs:unaryOpExpr(
          abs:negativeOp(location=cond.location), 
          cond, 
          location=cond.location), 
        body ) 
    );
}

