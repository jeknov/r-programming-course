complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  id2=id
  cases_no=0
  n<-length(id)
  m<-matrix(nrow=n, ncol=2)
  
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
    
    good<-complete.cases(data)
    data<-data[good,]   #reads data with complete cases only
    l<-dim(data)[1]     #calculates the number of complete cases
    
    m[i,1]<-id[i]       #fills the matrix with data
    m[i,2]<-l
  }
  
  m<-data.frame(m)      #converts a full matrix to a dataframe
  colnames(m)<-c("id", "nobs") #adds column names
  m
}