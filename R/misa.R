
setClass(Class="inSApar", representation = representation(ppm.error='numeric', clust.rt = 'numeric', mz.error = 'numeric', ion.mode='character', n.cores='numeric', block.size='numeric'))

getError <- function(ppm.error, mass) { 
	return((round(mass)*ppm.error)/(10^6))
}
getPPM <- function(mass.error, mass) {
	return((mass.error*10^6)/round(mass))
}

getAccMzMatches <- function(refmz, expmz, ppmThrs){
	return(unlist(.Call("_misa_getAccMzMatches", refmz, expmz, trunc(ppmThrs), PACKAGE="misaR")))
}

loadDBblockInMemory <- function(fromMZ, toMZ, con, maindt){
	## Output:
	##	- dbMDFull: containing MID, PRECURSOR, MSMSID, ADDUCTTYPE and METABOLITE_NAME
	##	- dbMD: same as dbMDFull, but removing duplicated molids (to do faster searches). Then we access only to specific elements in smMatFull to retrive multiple specids for the same molid.
	##	- dbSpec: spectral data (0 and 10V) to match in-source fragments
	## 	- namesVect: vector of names. Not binded to dbMD to avoid coercing dbMD into a character matrix (would slow searching it)
#profvis({	
		
	localIndexes <- which(maindt$precursor >= fromMZ & maindt$precursor <= toMZ)
	if(length(localIndexes)==0){
		return(list(smMat=NULL))
	}
	
	## Customize the internal DB
	smMat <- maindt[localIndexes,,drop=FALSE]
	smMatFull <- smMat
	colnames(smMatFull) <- c('mid', 'precursor', 'msmsid', 'adducttype')
	smMat <- smMat[!duplicated(smMat[,1]),,drop=FALSE]
	smMat <- smMat[order(smMat[,1]),,drop=FALSE]	
			
	molidsChar <- paste(smMat$mid, collapse=',')
	dbQuery <- paste("SELECT * FROM main_db WHERE mid IN (", molidsChar, ')', sep='')
	resn <- suppressWarnings(DBI::dbGetQuery(con, dbQuery))
		
	namesVect <- resn$name
	molidVect <- resn$mid
		
	## Re-order
	smMatMIDInd <- which(smMat$mid %in% molidVect)
	if(length(smMatMIDInd)==0){
		return(list(smMat=NULL))
	}
	smMat <- smMat[smMatMIDInd,, drop=FALSE]
	
	## Retrive spectral data
	mzSpecidsChar <- paste(smMatFull$msmsid, collapse=',')
	dbQuery <- paste('SELECT * FROM msms_db WHERE msmsid IN (', mzSpecidsChar, ')', sep='')
	resPG <- suppressWarnings(DBI::dbGetQuery(con, dbQuery))
	if(nrow(resPG)==0){
		return(list(smMat=NULL))
	}else{			
		return(list(dbMD=smMat, dbMDFull=smMatFull, dbSpec=resPG, metaboliteNames=namesVect))
	}
	#})
}	

matchInSourceFrag <- function(candidateMolid, featureIndex, dbObj, annTab, gpar, acMin){
	## Output:
	##	- fragFeatures: the indexes in diffreport (annTab) that are in-source fragments of the queried feature/molid
	##	- nInSource: the  total number of fragments in the low energy METLIN's spectrum (0 and 10 V)
#profvis({
	## Finds the SPECIDs of candidate precursor matches
	specdIdsVect <- dbObj$dbMDFull[which(dbObj$dbMDFull[,1] %in% candidateMolid), 'msmsid', drop=FALSE]
	specIdsVectPG <-  which(dbObj$dbSpec[,'msmsid'] %in% as.numeric(unlist(specdIdsVect)))
		
	resP <- dbObj$dbSpec[specIdsVectPG,, drop=FALSE]
	if(nrow(resP)==0) {	
		return(list(fragFeatures=vector()))
	}
	
	empInSource <- resP$mz
	empInSourceInt <- resP$intensity
	
	## Finds features eluting around the queried feature.
	nearbyFeats <- which(annTab$rtmed>=annTab$rtmed[featureIndex] - gpar@clust.rt & annTab$rtmed<= annTab$rtmed[featureIndex] + gpar@clust.rt)
	nearbyFeats <- nearbyFeats[-which(nearbyFeats==featureIndex)]
	if(length(nearbyFeats)==0) {
		return(list(fragFeatures=vector()))
	}
	
	## Removes fragments under acquisition m/z, and above precursor m/z value
	includeInds <- empInSource>acMin & empInSource<(annTab$mzmed[featureIndex]-0.5)
	empInSource <- empInSource[includeInds]
	empInSourceInt <- empInSourceInt[includeInds]
	if(length(empInSource)==0) {
		return(list(fragFeatures=vector()))
	}

	## Grouping fragments across 0 and 10 V with the same m/z (within an error)
	dMat <- as.matrix(dist(empInSource))
	dMat[lower.tri(dMat, diag=TRUE)] <- NA
	dupInds <- which(dMat<gpar@mz.error, arr.ind=TRUE)
	if(nrow(dupInds)!=0){
		remInds <- apply(dupInds, 1, function(x) x[which.min(empInSourceInt[x])])
		empInSource <- empInSource[-remInds]
		empInSourceInt <- empInSourceInt[-remInds]
	}	
	
	nInSource <- length(empInSource)
	if(nInSource==0) {
		return(list(fragFeatures=vector()))
	}

	## Match the observed (experimental) features against empirical (library) fragments. 
	mzLeastError <- sapply(empInSource, function(x) which.min(abs(x - annTab$mzmed[nearbyFeats])))
	mzError <- abs(empInSource - annTab$mzmed[nearbyFeats[mzLeastError]])
	fragFeatures <- nearbyFeats[mzLeastError[which(mzError<gpar@mz.error)]]
	
	if(length(fragFeatures)==0) {
		return(list(fragFeatures=vector()))
	}
	
	## Dot-product score:
	## Select the columns with the feature areas (calculate pseudo-spectrum)
	ms1spectrum <- rep(0, 1200)	
	ms1REFspectrum <- ms1spectrum
	
	ms1spectrum[round(annTab$mzmed[fragFeatures])] <- rowSums(annTab[fragFeatures, -which(colnames(annTab) %in% c('featureidx', 'mzmed','rtmed')), drop=FALSE ])
	ms1REFspectrum[round(empInSource)] <- empInSourceInt 
		
	MF <- round(100*cor(ms1spectrum, ms1REFspectrum),0)
	
	return(list(fragFeatures=fragFeatures, nInSource=nInSource, MF=MF))
}	

formatData <- function(annTab, inSourceList, adductList, molidList, nFragList, adductTable){
	## Output:
	##	- resultsTable: formatted table of results (diffreport plus additional columns)
	
	inSourceVector <- rep('', nrow(annTab))	
	maxRatioVector <- rep('', nrow(annTab))	
	maxNVector <- rep('', nrow(annTab))	
	lossTxt <- rep('', nrow(annTab))	
	for(i in 1:nrow(annTab))
	{	
		mainMolid <- as.vector(na.omit(molidList[[i]]))
		fragsRatios <- as.vector(na.omit(nFragList[[i]]))
		adductTypes <- as.vector(na.omit(adductList[[i]]))

		addVctTxt <- character()
		maxRatio <- ''
		maxN <- ''
		if(length(mainMolid)!=0)
		{
			uniAT <- unique(adductTypes)
			addVctTxt <- sapply(1:length(uniAT), function(x) {
				eAT <- which(adductTypes %in% uniAT[x])
				aTxt <- adductTable[which(adductTable[,'adducttype']==uniAT[x]),'Adduct']
				molVectAndRatio <- sapply(eAT, function(y) paste(mainMolid[y], '(', fragsRatios[y], ')', sep=''))
				paste(aTxt, ' Name: ', paste(molVectAndRatio, collapse=', '))
				})
			addVctTxt <- paste(paste(addVctTxt, collapse=';'), '; ', sep='')

		}
		fragsOf <- ''
		if(length(as.vector(na.omit(inSourceList[[i]])))!=0) {
			fragsOf <- paste('In-source Frgs. of:', paste(as.vector(na.omit(inSourceList[[i]])), collapse='; '))
		}	
				
		inSourceVector[i] <- paste(addVctTxt, fragsOf, sep='')
		maxRatioVector[i] <- maxRatio
		maxNVector[i] <- maxN
	}	
	
	resultsTable <- cbind(annTab, inSourceVector, stringsAsFactors=FALSE)
	colnames(resultsTable)[ncol(resultsTable)] <- c('In-Source Annotation')
	
	return(resultsTable)
}	

inSourceAnnotation <- function(annTab, dbCred, gpar)
{	
	adductTable <- matrix(c(1:4,'[M+H]','[M-H]','[M+Na]','[M+Cl]'), ncol=2)
	colnames(adductTable) <- c('adducttype', 'Adduct')
	####################################
	
	## What is the rounding based on MZ.Error, to group the same fragments across 0 and 10 V with slightly different m/z.
	roundDigit <- length(which(strsplit(as.character(gpar@mz.error),'')[[1]] %in% '0'))
	## Acquisition minimum m/z
	acMin <- round(min(annTab$mzmed))
	
	inSourceList <- as.list(rep(NA, nrow(annTab)))
	adductList <- as.list(rep(NA, nrow(annTab)))
	molidList <- as.list(rep(NA, nrow(annTab)))
	nFragList <- as.list(rep(NA, nrow(annTab)))
	
	## BLOCK SEGMENTATION. Feature m/z are divided into 25 blocks. METLIN (only name, molid and prec.) is then loaded into memory and precursor queries are made using the loaded lib.
	mzSeq <- seq(min(annTab$mzmed), max(annTab$mzmed), length.out= gpar@block.size)

	con <- DBI::dbConnect(MySQL(), user=dbCred$user, password=dbCred$password, dbname=dbCred$dbname, host=dbCred$host) 
	maindt <- DBI::dbReadTable(con, "spectra_db")	
	DBI::dbDisconnect(con)
	
	maindt <- maindt[which(maindt$mode==gpar@ion.mode), , drop=FALSE]
	
	subIndexes <- getAccMzMatches(maindt$precursor, annTab$mzmed, round(gpar@ppm.error))
	maindt <- maindt[which(subIndexes==1),c('mid','precursor','msmsid','adducttype'),drop=FALSE]
	
	selCols <- c(which(colnames(annTab) %in% c('featureidx','mzmed','rtmed')), c(which(colnames(annTab) %in% 'postHoc') + 1): (which(colnames(annTab) %in% 'isotopes') - 1))
	
	annTab <- annTab[,selCols]
	
	for(k in 2:length(mzSeq)){
		
		## Requests all the Mids of metabolites falling within these m/z boundaries (plus ppm error)			
		mzError <- getError(gpar@ppm.error, mzSeq[k-1])
		mzLowerLimit <- mzSeq[k-1] - mzError
		mzError <- getError(gpar@ppm.error, mzSeq[k])
		mzUpperLimit <- mzSeq[k] + mzError

		queryFeats <- which(annTab$mzmed>mzLowerLimit & annTab$mzmed<mzUpperLimit)
		
		con <- DBI::dbConnect(MySQL(), user=dbCred$user, password=dbCred$password, dbname=dbCred$dbname, host=dbCred$host) 
		dbObj <- loadDBblockInMemory(fromMZ=mzLowerLimit, toMZ=mzUpperLimit, con=con, maindt=maindt)	
		DBI::dbDisconnect(con)
		if(is.null(dbObj$dbMD)) {
			next
		}

		for(i in queryFeats)
		{	
			## Finds candidate precursor matches (observed features against precursors in DB)
			featMass <- annTab$mzmed[i]
			mzError <- getError(gpar@ppm.error, featMass)
			mzLowerLimit <- featMass - mzError
			mzUpperLimit <- featMass + mzError
		
			## Is there a match?
			smInds <- which(dbObj$dbMD[,2]>mzLowerLimit & dbObj$dbMD[,2]<mzUpperLimit)
			if(length(smInds)==0){ 
				next		
			}
							
			## Selects unique matched MOLIDS to check if their fragments are present in the data
			uniMolids <- dbObj$dbMD[smInds,1]			
	
			for(j in 1:length(uniMolids))
			{				
				matchObj <- matchInSourceFrag(candidateMolid = uniMolids[j], i, dbObj, annTab, gpar, acMin)
				#matchObj <- list(fragFeatures=NULL)
				if(length(matchObj$fragFeatures)==0) {
					next
				}

				## If a match with fragments is found, store the information.				
				for(w in 1:length(matchObj$fragFeatures)) {
					inSourceList[[matchObj$fragFeatures[w]]] <- c(inSourceList[[matchObj$fragFeatures[w]]], dbObj$metaboliteNames[smInds[j]])
				}	
				adductList[[i]] <- c(adductList[[i]], unique(dbObj$dbMD[smInds[j],4]))
				molidList[[i]] <- c(molidList[[i]], dbObj$metaboliteNames[smInds[j]])
				nFragList[[i]] <- c(nFragList[[i]], paste(length(matchObj$fragFeatures), '/', matchObj$nInSource, '-', matchObj$MF, '%', sep=''))	

			}
		}
	}
	
	## FORMATTING INFORMATION:
	## Up to this point, all the computation is done. The following lines transform the retrieved data into the new diffreport with the additional column(s).
	
	## The objects containing the computed information are as follows:
	## NOTE: each item in the list corresponds to a feature in annTab (diffreport): In other words, the element i in annTab[i,] corresponds to adductList[[i]].
	##		- inSourceList: contains the metabolite name (if the features matches an in-source fragment)
	## 		- adductList: contains the adduct type
	## 		- molidList: contains the metabolite name (if the features matches its precursor)
	## 		- nFragList: contains the score (number of fragments out of the total number of fragments in the 0-10V spectra)
	 
	resultsTable <- formatData(annTab, inSourceList, adductList, molidList, nFragList, adductTable)
	return(resultsTable)	
}	
	
	
	