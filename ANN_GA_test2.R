require(genalg)
library(genalg)

require(neuralnet)
library(neuralnet)

main<-function(){
	st=Sys.time()
	print(paste("Start:",st,sep=" "))
	
	iter_number = 100
	pop = 20

	print(paste("Iter. number:",iter_number, sep=" "))
	print(paste("Pupolation:", pop, sep=" "))
	print(paste("Combinations to test:", pop*iter_number,sep=" "))

	GAmodel <- rbga.bin(
		size=12,
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
	validat_data=stan_data[551:654,2:8]
	colnames(validat_data)=col_Names[2:8]

	hidden_layer=c( binary_to_decimal(x[1:4]) , binary_to_decimal(x[5:8]) , binary_to_decimal(x[9:12]))

	NNet_hidden= c(hidden_layer [! hidden_layer %in% 0])
	
	print(noquote(" "))
	print(noquote("=============================================================="))
	print(noquote("Hidden neurons: "))
	print(NNet_hidden)
	print(noquote("Learning..."))

	MyNNet=neuralnet(
		formula = longr~fleav6+imp1+mip+njobs+pace3+partyid+zun,
		data = as.data.frame(train_data),
		hidden = NNet_hidden,
		learningrate = 0.01,
		algorithm = "backprop",
		err.fct = "sse",
		linear.output=FALSE, 
		likelihood=TRUE,
		startweights = 0.02,
		rep=2
		)

	results=evaluate_NN(MyNNet, validat_data, col_Names)
	
	exp_value=matrix(ncol=1,nrow=104,data=stan_data[551:654,1])
	nn_output=matrix(ncol=1,nrow=104,data=results)

	error=calculate_error(exp_value, nn_output, integer_check)
	
	print(noquote("Error: "))
	print(error)
	print(noquote("=============================================================="))

	save_data=paste(error, binary_to_decimal(x[1:4]), binary_to_decimal(x[5:8]), binary_to_decimal(x[9:12]), sep="\t")

	cat(save_data,file="G:/MASTER/Code/R/for testing/errorGA3.txt",sep="\n" ,append=TRUE)
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

binary_to_decimal<-function(x)
{
	n = 0
	for (i in seq(x)) {
		n = n + x[i]*2^(length(x)-i)
	}
	return(n)
}

getData <- function(){
	path="G:/MASTER/Code/R/for testing/inputGA2.txt"
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
	results=compute(NNet, as.data.frame(validation_set))
	return(results$net.result)
}
#=====================================================================================================================================

