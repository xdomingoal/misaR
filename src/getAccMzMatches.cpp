/*===========================================================================*/
/* runfunc - running window functions                                        */
/* Adapted from Jarek Tuszynski functions in caTools (archived on CRAN)      */
/* Distributed under GNU General Public License version 3                    */
/*===========================================================================*/

// #include <stdlib.h>
// #include <stdio.h>
// #include <memory.h>
// #include <math.h>
// #include <float.h>

// #include <R.h>
// #include <Rinternals.h>

#include <Rcpp.h>
using namespace Rcpp;

//How to compilte a Rcpp function:
// 1. Edit code
// 2. Execute: compileAttributes(pckg_dir), from R

/*==================================================================================*/
/* Mean function applied to (running) window. All additions performed using         */
/* addition algorithm which tracks and corrects addition round-off errors (see      */  
/*  http://www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps)*/
/* Input :                                                                          */
/*   In   - array to run moving window over will remain umchanged                   */
/*   Out  - empty space for array to store the results                              */
/*   nIn  - size of arrays In and Out                                               */
/*   nWin - size of the moving window                                               */
/* Output :                                                                         */
/*   Out  - results of runing moving window over array In and colecting window mean */
/*==================================================================================*/

// [[Rcpp::export]]
List getAccMzMatches(NumericVector& refmz, NumericVector& expmz, NumericVector& ppmThrs) {
	NumericVector expmz_c  = expmz;
	NumericVector refmz_c  = refmz;
	NumericVector ppmThrs_c  = ppmThrs;
	
	int n = refmz_c.size();
	NumericVector ppm_error(n);
	NumericVector in_database(n, FALSE);

	for (int j=0; j<n; j++) {
    	ppm_error = (abs(refmz_c[j] - expmz_c)*1000000)/refmz_c[j];
      	//std::sort(ppm_error.begin(), ppm_error.end()); 
      	bool b = any(min(ppm_error) <= ppmThrs_c).is_true();
		in_database[j] = b;
    }
	
	return Rcpp::wrap(in_database);
}


// void getAccMzMatches(double *refmz, double *expmz, double *Out, int *ppmThrs, const int *nRef)
// { /* medium size version with NaN's and edge calculation, but only one level of round-off correction*/
//   int n=*nRef, ppmThrs_c=*ppmThrs;
//   double *out=Out, *expmz_c=expmz, *refmz_c=refmz;

//   for (int j=0; j<n; j++) {
//      double *ppm_error = (abs(refmz_c[j] - expmz_c)*1000000)/refmz_c[j];
//      bool b = any(min(ppm_error) <= ppmThrs_c).is_true();
//      *out[j] = b;
//    }
  
// }

