grammar edu:umn:cs:melt:ableC:artifacts:with_regex;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;

import edu:umn:cs:melt:ableC:drivers:batch ;

parser theParser :: cst:Root {
  edu:umn:cs:melt:ableC:concretesyntax ;
  edu:umn:cs:melt:exts:ableC:regex:regexLiterals ;
  edu:umn:cs:melt:exts:ableC:regex:regexMatching ;
} 

function main
IOVal<Integer> ::= args::[String] io_in::IO
{
  return driver(args, io_in, theParser);
}
