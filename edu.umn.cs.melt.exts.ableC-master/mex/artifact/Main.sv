
imports edu:umn:cs:melt:ableC:concretesyntax as cst;

import edu:umn:cs:melt:exts:ableC:mex:driver;

parser theParser :: cst:Root {
  edu:umn:cs:melt:ableC:concretesyntax ;
  edu:umn:cs:melt:exts:ableC:mex:concretesyntax ;
} 

function main
IOVal<Integer> ::= args::[String] io_in::IO
{
  return driver(args, io_in, theParser);
}

