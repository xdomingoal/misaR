\name{inSourceAnnotation}
\alias{inSourceAnnotation}
\title{In Source Fragment Annotation}
\description{Annotation of in-source fragments in a MySQL DB.}
\usage{
inSourceAnnotation(annTab, dbCred, gpar)
}
\arguments{
  \item{annTab}{Dataframe containig the resulting diffreport from XCMS.}
  \item{dbCred}{The database credentials (see examples)}
  \item{gpar}{MISA parameters, see examples.} 
}
\references{
[1] Xavier Domingo-Almenara, et al., Autonomous METLIN-guided in-source fragmentation detection increases annotation confidence in untargeted metabolomics. Submitted (2018). }
\author{
Xavier Domingo-Almenara. xdomingo@scripps.edu
}

\examples{
\donttest{

library(misaR)

## SOFTWARE PARAMETERS:
gpar <- new("inSApar", ppm.error = 20, clust.rt = 2, mz.error = 0.01, ion.mode='+', block.size=10)
dbCred <- list(user='dbuser', password='test12', dbname='misa_db', host='localhost')
fileName <- 'testReport.tsv'
isRTinMinutes <- 'Y'
##

annTab <- read.delim(fileName)

if(isRTinMinutes=='Y') annTab[,'rtmed'] <- annTab[,'rtmed']*60

annTabu <- inSourceAnnotation(annTab=annTab, dbCred=dbCred, gpar=gpar)

write.csv(annTabu, file='annotatedDiffreport.csv')	

}
}


