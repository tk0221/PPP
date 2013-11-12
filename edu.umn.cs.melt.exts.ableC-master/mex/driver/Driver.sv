grammar edu:umn:cs:melt:exts:ableC:mex:driver;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;

imports silver:langutil;
imports silver:langutil:pp;

function driver
IOVal<Integer> ::= args::[String] ioIn::IO 
  theParser::(ParseResult<cst:Root>::=String String)
{
  local fileName :: String = head(args);
  local splitFileName :: Pair<String String> = splitFileNameAndExtension(fileName);
  local baseFileName :: String = splitFileName.fst;
  
  local cppFileName :: String = baseFileName ++ ".gen_cpp";
  local cFileName :: String = baseFileName ++ ".c";
  local mexoutFileName :: String = baseFileName ++ ".mexout";
  
  local isF :: IOVal<Boolean> = isFile(fileName, ioIn);

  -- Run C pre processor over the file.
  local mkCppFile :: IOVal<Integer> =
    system("cpp -E -C -I /home/tedinski/Downloads/MATLAB/R2013b/extern/include \"" ++ fileName ++ "\" > " ++ cppFileName, isF.io);

  -- Read the output of CPP and parse it.
  local text :: IOVal<String> = readFile(cppFileName, mkCppFile.io);

  local result :: ParseResult<cst:Root> = theParser(text.iovalue, cppFileName);

  local r_cst :: cst:Root = result.parseTree;

  local r_ast :: abs:Root = r_cst.ast;
  
  local wtFile :: IO = 
    writeFile(cFileName, 
      "#include <mex.h>\n#include <math.h>\n" ++ show(100, r_ast.pp), text.io);
  
  local mexFile :: IOVal<Integer> =
    system("cd " ++ cutDir(cFileName) ++ " && mex CFLAGS='-std=gnu1x -D_GNU_SOURCE -fexceptions -fPIC -fno-omit-frame-pointer -pthread' " ++ cutFile(cFileName) ++ " &> " ++ mexoutFileName, wtFile);

  return if null(args) || !endsWith(".cx", fileName) then
    ioval(print("Expected input .cx file.\n", ioIn), 5)
  else if !isF.iovalue then
    ioval(print("File \"" ++ fileName ++ "\" not found.\n", isF.io), 1)
  else if mkCppFile.iovalue != 0 then
    ioval(print("CPP call failed.\n", mkCppFile.io), 3)
  else if !result.parseSuccess then
    ioval(print(result.parseErrors ++ "\n", text.io), 2)
  else
    ioval(mexFile.io, mexFile.iovalue);
}

function cutDir
String ::= s::String
{
  local i :: Integer = lastIndexOf("/", s);
  return if i == -1 then "." else substring(0, i, s);
}
function cutFile
String ::= s::String
{
  return substring(lastIndexOf("/", s) + 1, length(s), s);
}


