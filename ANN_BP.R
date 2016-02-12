require(nnet)
library(nnet)

main<-function(){
	
	data=getData()		
	col_Names=colnames(data)
	data_matrix=data_table2matrix(data)

	stan_data=matrix(data=0)
	stan_data=standData(data_matrix)

	train_data=matrix(data=0)
	train_data=stan_data[1:550,]

	validat_data=matrix(data=0)
	validat_data=stan_data[551:654,]

	MyNNet=createAndTrainNN(train_data, col_Names)

	results=evaluate_NN(MyNNet, validat_data, col_Names)
	writeData(results)

}

getData <- function(){
	path="G:/MASTER/Code/R/for testing/input.txt"
	data1=read.table(file=path, header=TRUE, sep="\t")
	return(data1)
}

writeData <- function(x){
	path="G:/MASTER/Code/R/for testing/output.txt"
	write.table(x, file=path, row.names=FALSE, col.names="nn output")
}

data_table2matrix <- function(x)
{
	n_row=dim(x)[1]
	n_col=dim(x)[2]
	m=matrix(ncol= n_col, nrow= n_row, data=0)
	
	for (i in seq(n_row)) {
		for (j in seq(n_col)) {
			m[i,j]=x[i,j]
		}
	}

	return(m)
}

standData <- function(DataToStand){
	n_col=dim(DataToStand)[2]
	n_row=dim(DataToStand)[1]

	low_bond=0.1
	up_bond=0.9

	standDataTable=matrix(ncol=n_col,nrow=n_row,data=0)

	for (i in seq(n_col))
	{
		min_value=min(DataToStand[,i])
		max_value=max(DataToStand[,i])

		for (j in seq(n_row)) {
			standDataTable[j,i]= low_bond + ( up_bond - low_bond ) * ( (DataToStand[j,i]-min_value)/(max_value-min_value))
		}
	}
	return(standDataTable)
}

createAndTrainNN <- function(standInputData,columnNames){
	colnames(standInputData)=columnNames
	MyNNet=nnet(
		formula = 
		# GA:7
		longr ~ fleav6 + imp1 + mip + njobs + pace3 + partyid + zun,
		#0.1:13	
		#longr ~ cmon + find + fleav1 + griev2 + imp1 + jobsat + marital + pay + pf2 + prom2 + retage + rocc + zun,
		#0.05:29	
		#longr ~ auto1 + bencut + born + cc3 + cellpgr1 + cmon + cprob1 + find + fleav1 + flex2 + griev2 + gvt03 + imp1 + injrd + isum + jobsat + layoff + marital + mip + month + partyid + pay + pf2 + prom2 + retage + rocc + tcnt + train2a + zun ,
		#all:57 	
		#longr ~ auto1 + bencut + born + cc3 + cellpgr1 + cmhow + cmon + cmtim + cprob1 + dline + find + fleav1 + fleav6 + flex2 + free + griev2 + gvt02 + gvt03 + gvt06 + gvt07 + hear + howpaid + ideo + imp1 + injrd + isum + jobsat + layoff + live + lose1 + marital + mip + month + njobs + nphones + nwrkrs1 + observ1 + pace3 + partyid + pay + pf2 + pnm1 + prob4 + prob5 + prob6 + prom2 + raft +  retage +  rocc + s1hx + stime + tcnt + tel1 + time + train2a + whatif + zun,

		data=standInputData,
		MaxNWts = 10000,
		size=5,
		entropy=T,
		abstol=0.01,
		decay = 5e-4, 
		maxit = 2000  
		)
	return(MyNNet)
}

evaluate_NN <- function(NNet, validation_set, columnNames){
	colnames(validation_set)=columnNames
	results=predict(NNet, validation_set)
	return(results)
}
