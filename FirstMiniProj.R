## unzipping the file
zipF<- "C:\\Users\\Pc\\Documents\\specdata.zip"
outDir<-"C:\\Users\\Pc\\Documents\\specdata"
unzip(zipF,exdir=outDir)

#########

list.files ("specdata")

######### PROBLEM 1

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

######### PROBLEM 2

complete <- function(directory, id = 1:332)
{
  #Create a list of file
  files_full <- list.files(directory, full.names= TRUE)
  # Create empty data frame 
  dat <- data.frame()
  for (i in id)
  {
    # Read files
    temp <- read.csv(files_full[i])
    # nobs are sum of all complete cases
    nobs <-sum(complete.cases(temp))
    # Enamurtates complete cass by index
    dat <-rbind(dat, data.frame(i, nobs))
    
  }
  colnames(dat) <- c("id", "nobs")
  return(dat)
}
#outcome
complete("specdata", 1)

####### PROBLEM 3

corr <- function(directory, threshold = 0) {
  df = complete(directory)
  ids = df[df["nobs"] > threshold, ]$id
  corr = numeric()
  for (i in ids) {
    
    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    dff = newRead[complete.cases(newRead), ]
    corr = c(corr, cor(dff$sulfate, dff$nitrate))
  }
  return(corr)
}
complete <- function(directory, id = 1:332) {
  f <- function(i) {
    data = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                          ".csv", sep = ""))
    sum(complete.cases(data))
  }
  nobs = sapply(id, f)
  return(data.frame(id, nobs))
}
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)

###### PROBLEM 4

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
# Coerce character into numeric
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
# install.packages("data.table")
library("data.table")

# Reading in data
outcome <- data.table::fread('outcome-of-care-measures.csv')
outcome[, (11) := lapply(.SD, as.numeric), .SDcols = (11)]
# Modified code
outcome[, lapply(.SD
                 , hist
                 , xlab= "Deaths"
                 , main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
                 , col="lightblue")
        , .SDcols = (11)]