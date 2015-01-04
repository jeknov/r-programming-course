corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  cases_no=0
  crl<-vector(length=332)
  
  for (i in 1:332){
    ## Updates the name of the monitor:
    id_vec<-c("0","0",i)
    
    if(i<10){
      id<-paste(id_vec[1:3],collapse="")
    }
    else if(i >9 & i<100){
      id<-paste(id_vec[2:3],collapse="")
    }
    else{
      id<-i
    }
    
    ## Read values
    src_v<-c(directory,"/",id,".csv")
    src_name<-paste(src_v,collapse="")
    data<-read.csv(src_name)
    
    good<-complete.cases(data)
    data<-data[good,]   #reads data with complete cases only
    l<-dim(data)[1]     #calculates the number of complete cases
    
    if (l>threshold){
      crl[i]<-cor(data[,2],data[,3])
      #print(crl)
    }
  }
  #good_cr<-complete.cases(crl)
  crl<-crl[crl!=0]
  #print(crl)
}