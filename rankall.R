rankall <- function(outcome, num = "best") {
        ## Read outcome data
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        cdata <- as.data.frame(cbind(data[, 2],   # hospital
                                     data[, 7],   # state
                                     data[, 11],  # heart attack
                                     data[, 17],  # heart failure
                                     data[, 23]),  # pneumonia
                               stringsAsFactors = FALSE)
        colnames(cdata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        Hstates <- unique(data[,7])
        state <- Hstates[order(Hstates)]
        
        ## Check that state and outcome are valid
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                stop('invalid outcome')

        ## For each state, find the hospital of the given rank
        hospital <- character()
        for(state_i in state){
                hos <- rankhospital(state_i, outcome, num)
                hospital <- c(hospital, hos)
        }
        cbind.data.frame(hospital, state)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}