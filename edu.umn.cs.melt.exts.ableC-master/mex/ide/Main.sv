grammar edu:umn:cs:melt:exts:ableC:mex:ide;

imports edu:umn:cs:melt:exts:ableC:mex:artifact;
imports ide;


temp_imp_ide_dcl theParser ".cx" {
  product {
    name "AbleC";
    version "0.0.2";
  }
  builder analyze;
};

function analyze
IOVal<[IdeMessage]> ::= args::[IdeProperty] env::IdeEnv i::IO
{
  local root :: String = env.projectPath ++ "/";
  local allFilesInProject :: IOVal<[String]> = listContents(root, i);
  local cxfiles :: [String] = filter(endsWith(".cx", _), allFilesInProject.iovalue);
  local cxpaths :: [String] = map(strApp(root, _), cxfiles);
  
  return mainThemAll(cxpaths, allFilesInProject.io);
}

function mainThemAll
IOVal<[IdeMessage]> ::= cxpaths::[String]  ioin::IO
{
  return if null(cxpaths) then ioval(ioin, [])
  else mainThemAll(tail(cxpaths), doMain(head(cxpaths), ioin));
}

function doMain
IO ::= cxpath::String  ioin::IO
{
  local m :: IOVal<Integer> = main([cxpath], ioin);

  local m2 :: IO = 
    if m.iovalue != 0 then
      perror("Failure in main call.\n", m.io)
    else 
      m.io;

  local splitFileName :: Pair<String String> = splitFileNameAndExtension(cxpath);
  local mexout :: String = splitFileName.fst ++ ".mexout";
  
  local fe :: IOVal<Boolean> = isFile(mexout, m2);
  
  local con :: IOVal<String> = readFile(mexout, fe.io);
  
  local printerrors :: IO = perror(con.iovalue, con.io);
  
  return if fe.iovalue then
    if con.iovalue != "" then
      printerrors
    else
      con.io
  else
    fe.io;
}

function strApp
String ::= s::String  d::String
{ return s ++ d; }
