grammar edu:umn:cs:melt:ableC:artifacts:with_matrix;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;

import edu:umn:cs:melt:ableC:drivers:parseAndPrint ;

parser theParser :: cst:Root {
  edu:umn:cs:melt:ableC:concretesyntax ;
  edu:umn:cs:melt:exts:ableC:interval ;
  edu:umn:cs:melt:exts:ableC:matrix ;
  edu:umn:cs:melt:exts:ableC:matrix:print ;
  edu:umn:cs:melt:exts:ableC:matrix:index ;
  edu:umn:cs:melt:exts:ableC:matrix:intervalIndex ;
  edu:umn:cs:melt:exts:ableC:matrix:decl ;
} 

function main
IOVal<Integer> ::= args::[String] io_in::IO
{
  return driver(args, io_in, theParser);
}

