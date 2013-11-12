grammar edu:umn:cs:melt:ableC:drivers:parseOnly ;

import edu:umn:cs:melt:ableC:concretesyntax as cst ;

function driver
IOVal<Integer> ::= args::[String]  ioIn::IO
  the_parser::(ParseResult<cst:Root>::=String String)
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

  local result :: ParseResult<cst:Root> = the_parser(text.iovalue, cppFileName);

  return if !isF.iovalue then
    ioval(print("File \"" ++ fileName ++ "\" not found.\n", isF.io), 1)
  else if mkCppFile.iovalue != 0 then
    ioval(print("CPP call failed.\n", mkCppFile.io), 3)
  else if !result.parseSuccess then
    ioval(print(result.parseErrors ++ "\n", text.io), 2)
  else
    ioval(text.io, 0);
}

