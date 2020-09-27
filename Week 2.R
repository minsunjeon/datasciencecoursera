specdata <- getwd()
#function 1-calculates the mean of a pollutant across a specified list of monitors
pollutantmean <- function(directory, pollutant, id = 1:332) {
  #create a list of files
  files<-list.files(path= specdata,pattern = ".csv", full.names = TRUE)
  #create an empty data frame
  dat <-data.frame()
  #loop through the list of files until id is found
  for (i in id){
   #read in the file and add files to the main data frame
     dat <- rbind(dat,read.csv(files[i]))
}
  #find the mean of the pollutant, make sure you remove NA values
  return(mean(dat[,pollutant],na.rm=TRUE))
}


pollutantmean(specdata, "sulfate", 1:10)

#function 2-reads a directory full of files and reports the number of completely observed cases in each data file
complete <- function(directory,id = 1:332) {
  #create a list of files
  files <-list.files(path=specdata,full.names = TRUE)
  #create an empty data frame
  dat <-data.frame()
  for (i in id) {
  #read in the file
    temp <- read.csv(files[i], header=TRUE)
  #delete rows that do not have complete cases
    temp <- na.omit(temp)
  #count all of the rows with complete cases
    nobs <- nrow(temp)
  #enumerate the complete cases by index
    dat <- rbind(dat, data.frame(i,nobs))
  }
  return(dat)
}
complete(specdata, c(2, 4, 8, 10, 12))

#function 3-takes a directory of data files and a threshold for complete cases 
#and calculates the correlation between sulfate & nitrate for monitor locations
#where the number of completely observed cases is greater than the threshold
corr <-function(directory,threshold = 0){
  #create a list of files
  files <-list.files(path=specdata,full.names = TRUE)
  #create empty vector
  dat <- vector(mode = "numeric", length = 0)
  
  for(i in 1:length(files)){
    #read in file
    temp <-read.csv(files[i], header = TRUE)
    #delete NAs
    temp <-temp[complete.cases(temp),]
    #count the number of observations
    csum <-nrow(temp)
    #if the nubmer of rows is greater than the threshold
    if(csum > threshold){
      dat <- c(dat,cor(temp$nitrate, temp$sulfate))
    }
  }
  return(dat)
}
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
cc <- complete(specdata, c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))