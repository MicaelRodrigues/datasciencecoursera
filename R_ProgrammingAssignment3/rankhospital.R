rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("r3_data/outcome-of-care-measures.csv", colClasses = "character")
    data <- data[c(2, 7, 11, 17, 23)]
    ## Simplify outcome filtering
    names(data)[3] <- "heart attack"
    names(data)[4] <- "heart failure"
    names(data)[5] <- "pneumonia"
    
    ## Check that state and outcome are valid
    states = unique(data[,2])
    if( state %in% states == FALSE) stop("invalid state")
    
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
    
    if(!num %in% c("best","worst") && num%%1 != 0 ) stop("invalid num")
    
    ## Filter data by State
    data <- data[data$State == state & data[outcome] != 'Not Available',]
    ## Order the data
    data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
    data <- data[order(data$Hospital.Name, decreasing = FALSE), ]
    data <- data[order(data[outcome], decreasing = FALSE), ]
    
    ## Get Rates by desired Outcome
    rates <- data[, outcome]
    
    if( num == "best" ) {
        rowIndex <- which.min(rates)
    } else if( num == "worst" ) {
        rowIndex <- which.max(rates)
    } else {
        rowIndex <- num
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    data[rowIndex, ]$Hospital.Name
}