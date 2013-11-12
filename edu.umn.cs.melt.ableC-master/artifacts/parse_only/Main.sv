grammar edu:umn:cs:melt:ableC:artifacts:parse_only;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;

import edu:umn:cs:melt:ableC:drivers:parseOnly ;

parser theParser :: cst:Root {
  edu:umn:cs:melt:ableC:concretesyntax ;
} 

function main
IOVal<Integer> ::= args::[String] io_in::IO
{
  return driver(args, io_in, theParser);
}

