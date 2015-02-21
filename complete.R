# Programming Assignment 1: Air Pollution: Instructions


complete <- function(directory = "specdata", id = 1:332) {
        
        tab <- data.frame(id = NULL, nobs = NULL)         # создаем таблицу с колонками "id", "nobs"

        for(i in id){
                id_str <- as.character(i)
                if(nchar(id_str[1]) == 1){
                        id_str[1] <- paste("00", id_str[1], sep = "") # если i однозначное - добавить два ноля
                }else{
                        if(nchar(id_str[1])==2){
                                id_str[1] <- paste("0", id_str[1], sep = "")
                        }
                }
                name <- character(1)    # создаем вектор
                print(id_str)
                name <- paste(directory, "/", id_str, ".csv", sep = "") # передаём вектору имя файла
                        print(name)
                data <- read.csv(file = name) # читаем файл
                naSulfate <- is.na(data$sulfate) 
                naNitrate <- is.na(data$nitrate)
                nnobs <- naNitrate == FALSE & naSulfate == FALSE
                nobsSum <- sum(nnobs)
                        print(nobsSum)
                row <- list(i, nobsSum)
                names(row) <- c("id", "nobs")
                tab <- rbind.data.frame(tab, row)
                rm(id_str)
                }
        print(tab)
        }


        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

