pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    #data = numeric()
    #for (i in id) {#iterate through the supplied ids
    #    File = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
    #                                                        ".csv", sep = ""))
    #    message(as.character(class(File)))
    #    data = c(data, File[[pollutant]])
    #}
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    #return(mean(data, na.rm = TRUE))
    data = lapply(id, function(i) read.csv(paste(directory, "/", formatC(i, 
                    width = 3, flag = "0"), ".csv", sep = ""))[[pollutant]])
    
    return(mean(unlist(data), na.rm = TRUE))
}