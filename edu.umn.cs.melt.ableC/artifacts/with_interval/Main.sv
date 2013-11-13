grammar edu:umn:cs:melt:ableC:artifacts:with_interval;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;

import edu:umn:cs:melt:ableC:drivers:parseAndPrint ;

parser theParser :: cst:Root {
  edu:umn:cs:melt:ableC:concretesyntax ;
  edu:umn:cs:melt:exts:ableC:interval ;
} 

function main
IOVal<Integer> ::= args::[String] io_in::IO
{
  return driver(args, io_in, theParser);
}

{-
copper_mda test(theParser) {
  edu:umn:cs:melt:exts:ableC:matrix;
}
-}