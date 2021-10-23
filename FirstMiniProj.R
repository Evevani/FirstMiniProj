## unzipping the file
zipF<- "C:\\Users\\Pc\\Documents\\CMSC197\\specdata.zip"
outDir<-"C:\\Users\\Pc\\Documents\\CMSC197\\specdata"
unzip(zipF,exdir=outDir)

#########
setwd("C:\\Users\\Pc\\Documents\\CMSC19")
list.files ("specdata")

#########

pollutantmean <- function(directory, pollutant, id = 1:332)
{
  #create a list of files
  files_full <- list.files(directory, full.names = TRUE) 
  # create an empty data frame
  dat <- data.frame()
  for (i in id)
  {
    #add files to main data
    dat <- rbind(dat, read.csv(files_full[i]))
    
  }
  #Calulate mean
  mean_data <- mean(dat[, pollutant], na.rm = TRUE)
  return(mean_data)
}
pollutantmean("specdata","sulfate",1:10)
