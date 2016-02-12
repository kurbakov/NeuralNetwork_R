
main<-function(){

	data_raw=getData()		
	col_Names=colnames(data)

	data1=data_raw[551:654,]

	lrfit=glm(formula = 
		#0.1:13	
		#longr ~ cmon + find + fleav1 + griev2 + imp1 + jobsat + marital + pay + pf2 + prom2 + retage + rocc + zun,
		#0.05:29	
		#longr ~ auto1 + bencut + born + cc3 + cellpgr1 + cmon + cprob1 + find + fleav1 + flex2 + griev2 + gvt03 + imp1 + injrd + isum + jobsat + layoff + marital + mip + month + partyid + pay + pf2 + prom2 + retage + rocc + tcnt + train2a + zun ,
		#all:57 	
		#longr ~ auto1 + bencut + born + cc3 + cellpgr1 + cmhow + cmon + cmtim + cprob1 + dline + find + fleav1 + fleav6 + flex2 + free + griev2 + gvt02 + gvt03 + gvt06 + gvt07 + hear + howpaid + ideo + imp1 + injrd + isum + jobsat + layoff + live + lose1 + marital + mip + month + njobs + nphones + nwrkrs1 + observ1 + pace3 + partyid + pay + pf2 + pnm1 + prob4 + prob5 + prob6 + prom2 + raft +  retage +  rocc + s1hx + stime + tcnt + tel1 + time + train2a + whatif + zun,

		data = as.data.frame(data1),
		family = binomial(logit))

	forecast=predict(lrfit, type="response")
	
	writeData(forecast)
}

getData <- function(){
	path="G:/MASTER/Code/R/for testing/inputLR.txt"
	data1=read.table(file=path, header=TRUE, sep="\t")
	return(data1)
}

writeData <- function(x){
	path="G:/MASTER/Code/R/for testing/output.txt"
	write.table(x, file=path, row.names=FALSE, col.names="nn output")
}
