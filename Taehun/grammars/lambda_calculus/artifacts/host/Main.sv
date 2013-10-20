grammar lambda_calculus:artifacts:host ;

import lambda_calculus:host:concretesyntax as cnc ;
import lambda_calculus:host:abstractsyntax as abs ;

parser the_parser :: cnc:Root {
  lambda_calculus:host:concretesyntax ;
}

function main
IOVal<Integer> ::= args::[String] io_in::IO
{
  local attribute filename :: String = head(args) ;

  production attribute isF :: IOVal<Boolean>
    = isFile(filename, io_in);
   
  production attribute text :: IOVal<String>
    = readFile(filename, isF.io);

  local attribute result :: ParseResult<cnc:Root>
    = the_parser(text.iovalue, filename);

  local attribute r_cst :: cnc:Root;
  r_cst = result.parseTree;

  local attribute r_ast :: abs:Root;
  r_ast = r_cst.cnc:ast_Root ;

  local attribute pp_msg :: IO =
    print ("pretty print: \n" ++ r_cst.cnc:pp ++ "\n\n" ++
           "on ast: \n" ++ r_ast.abs:pp ++ "\n\n" 
           ,
           text.io) ;

  local attribute errors_msg :: IO =
    if   null (r_ast.abs:errors)
    then print ("No Errors!\n", pp_msg )
    else print ("Errors:\n" ++
                foldr ( stringConcat, "\n", r_ast.abs:errors ),
                pp_msg ) ;

  local attribute writeHaskell :: IO =
    if   null (r_ast.abs:errors)
    then writeFile ("LambdaTranslation.hs",
                    r_ast.abs:haskell, errors_msg ) 
    else errors_msg ;

  return if   ! isF.iovalue 
         then ioval( print( "File \"" ++ filename ++ "\" not found.\n\n", 
                       isF.io), 1 )
         else
         if   ! result.parseSuccess 
         then ioval( print( "File had parse errors:\n" ++
                            result.parseErrors ++ "\n\n", text.io ) ,
                     2 )
         else ioval( print( "Finished.\n\n", writeHaskell ) , 0);
}
