require(genalg)
library(genalg)

require(nnet)
library(nnet)

main<-function(){
	st=Sys.time()
	print(paste("Start:",st,sep=" "))
	
	iter_number = 100
	pop = 50
	
	print(paste("Iter. number:",iter_number, sep=" "))
	print(paste("Pupolation:", pop, sep=" "))
	print(paste("Combinations to test:", pop*iter_number,sep=" "))

	GAmodel <- rbga.bin(
		size=57,
		iters=iter_number,
		popSize=pop, 
		mutationChance=0.01, 
		evalFunc=evaluation_function)

	summary(GAmodel)
	plot(GAmodel)

	end=Sys.time()
	print(paste("End:",end,sep=" "))
	print(paste("Computation time:",end-st,sep=" "))
}

evaluation_function<-function(x){

	if(sum(x)==0)
	{
		return(105)
	}
	
	data=getData()		
	col_Names=colnames(data)
	data_matrix=data_table2matrix(data)

	integer_check = 0.1 + (0.9-0.1) * ((1-min(data_matrix[,1]))/(max(data_matrix[,1])-min(data_matrix[,1])))

	stan_data=matrix(data=0)
	stan_data=standData(data_matrix)

	train_data=matrix(data=0)
	train_data=stan_data[1:550,]
	colnames(train_data)=col_Names

	validat_data=matrix(data=0)
	validat_data=stan_data[551:654,]
	colnames(validat_data)=col_Names
	
	NNet_hidden=round(sum(x)*0.75,0)

	gen_formula=x
	NNet_formula=create_formula(gen_formula)
	
	print(noquote(" "))
	print(noquote("=============================================================="))
	print(noquote("Formula: "))
	print(NNet_formula)
	print(noquote("Hidden neurons: "))
	print(NNet_hidden)
	print(noquote("Learning..."))

	MyNNet=MyNNet=nnet(
		formula = formula(NNet_formula),
		data = train_data,
		MaxNWts = 100000,
		size = NNet_hidden,
		entropy = T,
		abstol = 0.01,
		decay = 5e-4, 
		maxit = 100 
		)

	results=evaluate_NN(MyNNet, validat_data, col_Names)
	
	exp_value=matrix(ncol=1,nrow=104,data=validat_data[,1])
	nn_output=matrix(ncol=1,nrow=104,data=results)

	error=calculate_error(exp_value, nn_output, integer_check)
	
	print(noquote("Error: "))
	print(error)
	print(noquote("=============================================================="))

	save_data=paste(error, NNet_hidden, NNet_formula, sep="\t")

	cat(save_data,file="G:/MASTER/Code/R/for testing/errorGA.txt",sep="\n" ,append=TRUE)
	return(error)
}

calculate_error <-function (x, y, integer_check){
	mm=matrix(ncol=5, nrow=104 ,data=0) # real val, result, real value 0 1, result 0 1, error
	mm[,1]=x
	mm[,2]=y

	checked_value=integer_check

	for (i in seq_len(dim(x)[1])) {
		if(mm[i,1] >= checked_value){
			mm[i,3] = 1
		}else(mm[i,3] = 0)

		if(mm[i,2] >= checked_value){
			mm[i,4] = 1
		}else(mm[i,4] = 0)
	}

	mm[,5]= sqrt( (mm[,3]-mm[,4])^2 )
	total_error=sum(mm[,5])
	
	return(total_error)
}

create_formula<-function(x){

	all=list("auto1", "bencut", "born", "cc3", "cellpgr1", "cmhow", "cmon", "cmtim", "cprob1", "dline", "find", "fleav1", "fleav6", "flex2", 
		"free", "griev2", "gvt02", "gvt03", "gvt06", "gvt07", "hear", "howpaid", "ideo", "imp1", "injrd", "isum", "jobsat", "layoff", "live", 
		"lose1", "marital", "mip", "month", "njobs", "nphones", "nwrkrs1", "observ1", "pace3", "partyid", "pay", "pf2", "pnm1", "prob4", 
		"prob5", "prob6", "prom2", "raft", "retage", "rocc", "s1hx", "stime", "tcnt", "tel1", "time", "train2a", "whatif", "zun")

	formula=character()
	counter=0

	for(i in seq(x)){		
		
		if(x[i] == 1 && counter == 1){
			formula = paste(formula, all[i], sep= " + ")
		}
		
		if(x[i] == 1 && counter == 0){
			formula = all[i]
			counter = 1
		}
	}
	formula=paste("longr ~ ", formula, sep="")

	return(noquote(formula))
}

getData <- function(){
	path="G:/MASTER/Code/R/for testing/input.txt"
	data1=read.table(file=path, header=TRUE, sep="\t")
	return(data1)
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

	low_bond=0
	up_bond=1

	standDataTable=matrix(ncol=n_col,nrow=n_row,data=0)

	for (i in seq(n_col))
	{
		min_value=min(DataToStand[,i])
		max_value=max(DataToStand[,i])

		for (j in seq(n_row)) {
			standDataTable[j,i] = low_bond + ( up_bond - low_bond ) * ( (DataToStand[j,i]-min_value)/(max_value-min_value))
		}
	}
	return(standDataTable)
}

evaluate_NN <- function(NNet, validation_set, columnNames){
	colnames(validation_set)=columnNames
	results=predict(NNet, validation_set)
	return(results)
}
#=====================================================================================================================================
