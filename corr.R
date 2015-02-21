corr <- function(directory = "specdata", threshold = 0) {
        
        c <- vector()
        
        for(i in 1:332){
                id_str <- as.character(i)
                if(nchar(id_str[1]) == 1){
                        id_str <- paste("00", id_str, sep = "") # если i однозначное - добавить два ноля
                }else{
                        if(nchar(id_str[1])==2){
                                id_str <- paste("0", id_str, sep = "")
                        }
                }
                name <- character(1)
                name <- paste(directory, "/", id_str, ".csv", sep = "") 
                #print(name)
                data <- read.csv(file = name)
                
                naSulfate <- is.na(data$sulfate) 
                naNitrate <- is.na(data$nitrate)
                nobs <- naNitrate == FALSE & naSulfate == FALSE
                nobsSum <- sum(nobs)
                
                if(nobsSum >= threshold){
                        c <- c(c, cor(data$sulfat[nobs], data$nitrate[nobs]))
                }
        }
        print(c)
        #print(length(c))
}


        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations




   <- function(directory = "specdata", pollutant = "sulfate", id = 1:332) {
        c <- vector()
        for(i in id){
                id_str <- as.character(i)
                if(nchar(id_str[1]) == 1){
                        id_str <- paste("00", id_str, sep = "") # если i однозначное - добавить два ноля
                }else{
                        if(nchar(id_str[1])==2){
                                id_str <- paste("0", id_str, sep = "")
                        }
                }
                name <- character(1)
                name <- paste(directory, "/", id_str, ".csv", sep = "") 
                #print(name)
                data <- read.csv(file = name)
                #print(head(data))
                c <- c(c, data[[pollutant]])
                #print(mean(c))
        }
        print(mean(c, na.rm = TRUE))
}