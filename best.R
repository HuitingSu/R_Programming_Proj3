best <- function(state, outcome) {
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
        
        # chosendata <- list(hospital = data[, 2],   # hospital
        #                    State = data[, 7],   # state
        #                    "heart attack" = data[, 11],  # heart attack
        #                    "heart failure" = data[, 17],  # heart failure
        #                    pneumonia = data[, 23])
        # Hstates <- unique(chosendata[[State]])
        
        ## Check that state and outcome are valid
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                stop('invalid outcome')
        if (!state %in% Hstates)
                stop('invalid state')
        
        # col <- if(outcome == "heart attack"){
        #         11
        # }else if(outcome == "heart failure") {
        #         17
        # }else {
        #         23
        # }
        
        
        good <- cdata$state == state
        temp <- cdata[good, ]   ## extract same state data
        temp[,outcome] <- as.numeric(temp[,outcome])
        min_val <- min(temp[,outcome], na.rm = TRUE)
        ## extract the hospitals with minimum value 
        result <- temp[, "hospital"][which(temp[,outcome] == min_val)]   
        result <- result[order(result)]   ##sort to break tie
        
        result[1]
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}