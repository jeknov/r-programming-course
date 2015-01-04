best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check if state is valid
  if (state %in% out[,7]){  # state value is right
    
    # Check if outcome is valid
    if (outcome=="heart attack" || outcome=="heart failure" || outcome=="pneumonia"){  
      
      # Assign the column number acc. to the outcome
      if (outcome == "heart attack") {outCol<-11}
      else if (outcome == "heart failure") {outCol<-17}
      else {outCol<-23}
      
      # Subset only the necessary data.
      out2<-out[,][out[,7]==state,c(2,7,outCol)]
      a<-as.numeric(out2[,3]) # convert death rates to numeric values
      minRow<-match(min(a[!is.na(a)]),a)  # calculate minimum of death rates and find an associated row number
      out2[minRow,1]  # print the name of the hospital
    }
    else {  # outcome value is wrong
      stop("invalid outcome")
    }
  }
    
  else{  # state value is wrong
    stop("invalid state")
  }
  
  
}