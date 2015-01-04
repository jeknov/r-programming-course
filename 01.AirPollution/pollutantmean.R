pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## Decide which data column to choose:
  if(pollutant=="sulfate"){
    col_no=2
  }
  else if(pollutant=="nitrate"){
    col_no=3
  }
  else{
    print("Pollutant name is wrong")
  }
  
  id2=id
  mean=0
  length=0
  n<-length(id)
  
  for (i in 1:length(id)){
    ## Updates the name of the monitor:
    id_vec<-c("0","0",id[i])
    
    if(id[i]<10){
      id2[i] <-paste(id_vec[1:3],collapse="")
    }
    else if(id[i] >9 & id[i] <100){
      id2[i] <-paste(id_vec[2:3],collapse="")
    }
    else{
      id2[i] <-id[i]
    }
    
    ## Read values
    src_v<-c(directory,"/",id2[i],".csv")
    src_name<-paste(src_v,collapse="")
    
    data<-read.csv(src_name)

    m<-data[,col_no]
    mean<-mean+sum(m,na.rm=TRUE)
    length<-length+length(m)-sum(is.na(m))
  }

  
  ## Calculate mean and ignore NA values
print(mean/length)
}