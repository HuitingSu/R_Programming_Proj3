rankhospital <- function(state, outcome, num = "best") {
       
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
        
        ## Check that state and outcome are valid
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                stop('invalid outcome')
        if (!state %in% Hstates)
                stop('invalid state')
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        right_state <- cdata$state == state
        temp <- cdata[right_state, ]   ## extract same state data
        temp[,outcome] <- as.numeric(temp[,outcome]) 
        good <- !is.na(temp[,outcome])
        temp <- temp[good,]
        orderh <- order(temp[,outcome], temp[,"hospital"])

        totalh <- max(orderh)
        if(num =="best") { num <- 1 }
        if (num == "worst"){num <- totalh}
        if (num > totalh){return('NA')}        
        
        result <- temp[orderh,]
        ##result <- temp[, "hospital"][which(orderh == num)]   
        result[num,"hospital"]
}
