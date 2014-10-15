corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    df = complete(directory)
    ids = df[df["nobs"] > threshold, ]$id
    correlation = numeric()
    for (i in ids) {
        
        File = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                 ".csv", sep = ""))
        df = File[complete.cases(File), ]
        correlation = c(correlation, cor(df$sulfate, df$nitrate))
    }
    return(correlation)
}