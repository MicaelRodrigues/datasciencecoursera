best <- function(state, outcome) {
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
    
    ## Filter data by State
    data <- data[data$State == state & data[outcome] != 'Not Available',]
    ## Get Rates by desired Outcome
    rates <- data[, outcome]
    ## Find row index
    rowIndex <- which.min(rates)
    ## Return hospital name in that state with lowest 30-day death rate
    data[rowIndex, ]$Hospital.Name
}