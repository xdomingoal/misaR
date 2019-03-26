# MISA R package

This package is the source code of the MISA algorithm, a strategy to annotate in-source fragments from the METLIN database. 

This repository containts the source code, and a small sample MySQL database, only for developers. Users are encouraged to use the online version of MISA available at http://xcmsonline.scripps.edu

If you use the package MISA in your analysis and publications please cite:

Domingo-Almenara, X., Montenegro-Burke, J. R., Guijas, C., Majumder, E. L-W., Benton, H. P., Siuzdak, G. Autonomous METLIN-guided in-source fragment detection increases annotation confidence in untargeted metabolomics. Analytical Chemistry 91 (2019) 3246–3253. DOI: 10.1021/acs.analchem.8b03126

## Getting Started

### Prerequisites

You need to have a MySQL installed in your computer. You can use the sample MySQL DB distributed with misaR, and install it in your computer. 

### Installing

You need to compile (requieres compilation using Rcpp) and install misaR.

```
R CMB build misaR
R CMD install  misaR_0.1.0.tar.gz 
```

## Running the tests

You can use a diffreport from XCMSOnline with misaR, an execute it via:


### Load package and define parameters

```
library(misaR)

## SOFTWARE PARAMETERS:
gpar <- new("inSApar", ppm.error = 20, clust.rt = 2, mz.error = 0.01, ion.mode='+', block.size=10)
dbCred <- list(user='dbuser', password='test12', dbname='misa_db', host='localhost')
fileName <- 'testReport.tsv'
isRTinMinutes <- 'Y'

```

### Read diffreport and convert minutes to seconds if necessary

```
annTab <- read.delim(fileName)
if(isRTinMinutes=='Y') annTab[,'rtmed'] <- annTab[,'rtmed']*60
```

### Execute MISA:

```
annTabu <- inSourceAnnotation(annTab=annTab, dbCred=dbCred, gpar=gpar)
```

## Authors

* **Xavier Domingo-Almenara** - *Initial work* 

## License

This project is licensed under the MIT License - see the LICENCE file for details



