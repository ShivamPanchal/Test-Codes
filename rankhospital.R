rankhospital <- function(state, outcome,  num = "best") {
            complete_data<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
            if (outcome== "heart attack") { 
                              column<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                                 }
            else if(outcome=="heart failure"){
                              column<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                                 }
            else if(outcome=="pneumonia"){
                              column<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                                 }
            else {
                   stop("invalid outcome")
                  }
            data_frame <- complete_data[complete_data$State == state, c("Hospital.Name", column)]
	
	      if (nrow(data_frame) == 0) {
		                stop("invalid state")	
	            }
          data_frame[,2] <- as.numeric(data_frame[,2])	
	ordered_data_frame <- order(data_frame[column], data_frame$Hospital.Name, na.last=NA)
	
	 if (num == "best") {
        as.character(data_frame$Hospital.Name[ordered_data_frame[1]])
    } else if (num == "worst") {
       as.character(data_frame$Hospital.Name[ordered_data_frame[length(ordered_data_frame)]])
    } else if (is.numeric(num)) {
       as.character(data_frame$Hospital.Name[ordered_data_frame[num]])
    } else {
        stop("invalid num")
    }


}