grammar edu:umn:cs:melt:ableC:artifacts:parse_and_print;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;

import edu:umn:cs:melt:ableC:drivers:parseAndPrint ;

parser theParser :: cst:Root {
  edu:umn:cs:melt:ableC:concretesyntax ;
} 

function main
IOVal<Integer> ::= args::[String] io_in::IO
{
  return driver(args, io_in, theParser);
}

{-

grammar edu:umn:cs:melt:ableC:artifacts:parse_and_print;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;

imports silver:langutil only ast, pp, errors, err, wrn;
imports silver:langutil:pp;

parser parse :: cst:Root {
  edu:umn:cs:melt:ableC:concretesyntax;
}

function main
IOVal<Integer> ::= args::[String] ioIn::IO
{
  local fileName :: String = head(args);
  local splitFileName :: Pair<String String> = splitFileNameAndExtension(fileName);
  local baseFileName :: String = splitFileName.fst;
  local cppFileName :: String = baseFileName ++ ".gen_cpp";
  
  local isF :: IOVal<Boolean> = isFile(fileName, ioIn);

  -- Run C pre processor over the file.
  local mkCppFile :: IOVal<Integer> =
    system("cpp -E -C \"" ++ fileName ++ "\" > " ++ cppFileName, isF.io);

  -- Read the output of CPP and parse it.
  local text :: IOVal<String> = readFile(cppFileName, mkCppFile.io);

  local result :: ParseResult<cst:Root> = parse(text.iovalue, cppFileName);

  local r_cst :: cst:Root = result.parseTree;

  local r_ast :: abs:Root = r_cst.ast;

  return if !isF.iovalue then
    ioval(print("File \"" ++ fileName ++ "\" not found.\n", isF.io), 1)
  else if mkCppFile.iovalue != 0 then
    ioval(print("CPP call failed.\n", mkCppFile.io), 3)
  else if !result.parseSuccess then
    ioval(print(result.parseErrors ++ "\n", text.io), 2)
  else
    ioval(print("HackyUnparse:\n" ++ hackUnparse(r_ast) ++ "\n\nPretty print:\n" ++
                show(1000,r_ast.pp) ++ "\n", text.io), 0);
}

-}
