rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("r3_data/outcome-of-care-measures.csv", colClasses = "character")
    data <- data[c(2, 7, 11, 17, 23)]
    ## Simplify outcome filtering
    names(data)[3] <- "heart attack"
    names(data)[4] <- "heart failure"
    names(data)[5] <- "pneumonia"
    
    ## Check that state and outcome are valid
    states = unique(data[,2])
    
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
    
    if(!num %in% c("best","worst") && num%%1 != 0 ) stop("invalid num")
    
    ## Filter data by State
    data <- data[data[outcome] != 'Not Available',]
    ## Order the data
    data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
    data <- data[order(data$Hospital.Name, decreasing = FALSE), ]
    data <- data[order(data[outcome], decreasing = FALSE), ]
    
    ## Helper functiont to process the num argument
    getHospitalByRank <- function(df, s, n) {
        df <- df[df$State==s, ]
        rates <- df[, outcome]
        if( n == "best" ) {
            rowIndex <- which.min(rates)
        } else if( n == "worst" ) {
            rowIndex <- which.max(rates)
        } else {
            rowIndex <- n
        }
        df[rowIndex, ]$Hospital.Name
    }
    
    ## For each state, find the hospital of the given rank
    newdata <- data.frame("hospital"=character(), "state"=character())
    for(st in states) {
        hosp <- getHospitalByRank(data, st, num)
        newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
    }
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
    newdata
}