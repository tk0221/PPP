grammar edu:umn:cs:melt:exts:ableC:mex:concretesyntax;

imports silver:langutil only ast, errors, err, wrn;
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as ast;

imports edu:umn:cs:melt:exts:ableC:mex:abstractsyntax as ast;

{-

matfun (double b[M][N]) = name(double a[M][N], double p = 2.0)
{
  for(int n = 0; n < N; n++) {
    double colnorm = 0.0;
    for(int m = 0; m < M; m++) {
      colnorm += pow(index(a[m][n]), p);
    }
    colnorm = pow(colnorm, 1.0/p);
    
    for(int m = 0; m < M; m++) {
      index(b[m][n]) = index(a[m][n]) / colnorm;
    }
  }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
        /* Macros for the ouput and input arguments */
        #define B_OUT plhs[0]
        #define A_IN prhs[0]
        #define P_IN prhs[1]
        double *B, *A, p, colnorm;
        int M, N, m, n;    
        if(nrhs < 1 || nrhs > 2) /* Check the number of arguments */
                mexErrMsgTxt("Wrong number of input arguments.");
        else if(nlhs > 1)
                mexErrMsgTxt("Too many output arguments.");    
               
        if(!IS_REAL_2D_FULL_DOUBLE(A_IN)) /* Check A */
                mexErrMsgTxt("A must be a real 2D full double array.");
        if(nrhs == 1) /* If p is unspecified, set it to a default value */
                p = 2.0;
        else /* If P was specified, check that it is a real double scalar */
                if(!IS_REAL_SCALAR(P_IN))
                        mexErrMsgTxt("P must be a real double scalar.");
                else
                        p = mxGetScalar(P_IN); /* Get p */
        M = mxGetM(A_IN); /* Get the dimensions of A */
        N = mxGetN(A_IN);
        A = mxGetPr(A_IN); /* Get the pointer to the data of A */
        B_OUT = mxCreateDoubleMatrix(M, N, mxREAL); /* Create the output matrix */
        B = mxGetPr(B_OUT); /* Get the pointer to the data of B */
        for(n = 0; n < N; n++) /* Compute a matrix with normalized columns */
        {
                for(m = 0, colnorm = 0.0; m < M; m++) colnorm += pow(A[m + M*n], p);
                colnorm = pow(fabs(colnorm),1.0/p); /* Compute the norm of the nth column */
                for(m = 0; m < M; m++) B[m + M*n] = A[m + M*n]/colnorm;
        }
        return;
}

-}

marking terminal MatlabFunction 'matfun' lexer classes {Ckeyword};

concrete productions top::ExternalDeclaration_c
| 'matfun' f::MexFunctionDeclaration_c
    { top.ast = f.ast; }


closed nonterminal MexFunctionDeclaration_c with location, ast<[ast:ExternalDecl]>;

concrete productions top::MexFunctionDeclaration_c
| '(' out::MexOutputDecls_c ')' '=' id::Identifier_t '(' inp::MexInputDecls_c ')' s::CompoundStatement_c
    { top.ast = [ast:mexExternalDecl(ast:mexFunction(ast:foldMexOutputDecl(out.ast), ast:fromId(id), ast:foldMexInputDecl(inp.ast), s.ast))]; 
    }


closed nonterminal MexOutputDecls_c with location, ast<[ast:MexDecl]>;
concrete productions top::MexOutputDecls_c
| h::MexDecl
    { top.ast = [h.ast]; }
| h::MexDecl ',' t::MexOutputDecls_c
    { top.ast = [h.ast] ++ t.ast; }

closed nonterminal MexInputDecls_c with location, ast<[ast:MexDecl]>;
concrete productions top::MexInputDecls_c
| h::MexDecl
    { top.ast = [h.ast]; }
| h::MexDecl ',' t::MexInputDecls_c
    { top.ast = [h.ast] ++ t.ast; }
| h::MexDecl ',' t::MexInputWithDefaultsDecls_c
    { top.ast = [h.ast] ++ t.ast; }

closed nonterminal MexInputWithDefaultsDecls_c with location, ast<[ast:MexDecl]>;
concrete productions top::MexInputWithDefaultsDecls_c
| h::MexDecl '=' e::ConstantExpr_c
    { top.ast = [case h.ast of ast:mexDecl(ty, id) -> ast:mexDeclDefaulted(ty, id, e.ast) end]; }
| h::MexDecl '=' e::ConstantExpr_c ',' t::MexInputWithDefaultsDecls_c
    { top.ast = [case h.ast of ast:mexDecl(ty, id) -> ast:mexDeclDefaulted(ty, id, e.ast) end] ++ t.ast; }

closed nonterminal MexDecl with location, ast<ast:MexDecl>;
concrete productions top::MexDecl
| sqs::SpecifierQualifierList_c  d::Declarator_c
    { sqs.givenQualifiers = sqs.typeQualifiers;
      d.givenType =
        ast:figureOutTypeFromSpecifiers(sqs.location, sqs.typeQualifiers, sqs.preTypeSpecifiers, sqs.realTypeSpecifiers, sqs.mutateTypeSpecifiers);
      top.ast = ast:mexDecl(d.ast, d.declaredIdent);
    }


--------------------------------------------------------------------------------

marking terminal MatlabIndex 'index' lexer classes {Ckeyword};

concrete productions top::PostfixExpr_c
| 'index' i::MexIndexExpr_c
    { top.ast = i.ast; }

closed nonterminal MexIndexExpr_c with location, ast<ast:Expr>;
concrete productions top::MexIndexExpr_c
| '(' id::Identifier_t ind::MexIndexes_c ')'
    { top.ast = ast:mexIndex(ast:fromId(id), (ind.ast), location=top.location); }

closed nonterminal MexIndexes_c with location, ast<[ast:Expr]>;
concrete productions top::MexIndexes_c
| h::MexIndex_c
    { top.ast = [h.ast]; }
| h::MexIndex_c  t::MexIndexes_c
    { top.ast = h.ast :: t.ast; }

closed nonterminal MexIndex_c with location, ast<ast:Expr>;
concrete productions top::MexIndex_c
| '[' e::AssignExpr_c ']'  -- AssignExpr, not expr, to avoid confusion. Doesn't allow , operator. (without a parens)
    { top.ast = e.ast; }


