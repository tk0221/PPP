grammar edu:umn:cs:melt:ableC:artifacts:ide_basic;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;

import edu:umn:cs:melt:ableC:drivers:parseOnly ;

--Andrew
import silver:langutil;
import ide;
--


parser theParser :: cst:Root {
  edu:umn:cs:melt:ableC:concretesyntax ;
} 

function main
IOVal<Integer> ::= args::[String] io_in::IO
{
  return driver(args, io_in, theParser);
}

--Andrew's Part
temp_imp_ide_dcl theParser ".cx"{
  product {
    name "CX";
    version "0.0.1";
  }
};
