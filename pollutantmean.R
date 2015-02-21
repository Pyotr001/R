# Programming Assignment 1: Air Pollution: Instructions

pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:332) {
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


  
  # конвертирует векторы в текстовые переменные и объединяет их
  # в одно текстовое выражение; аргумент sep позволяет задать
  # текстовое выражение, которое будет разделять значения объединяемых
  # векторов (по умолчанию это пробел)
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
