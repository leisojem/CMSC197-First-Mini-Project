###LIAN MARIE S. MEJOS
###CMSC 197 First Mini Project


#################### Problem 1 ####################

##Setting the working directory
  setwd("C:/Users/Lian Marie Mejos/Desktop")

##Creating a function pollutantmean
  pollutantmean <- function(directory, pollutant, id = 1:332) {

##Creating a list of file and storing it to variable all_files
  all_files <- list.files(directory, full.names = T) 
  
##Creating an empty data frame data
  data <- data.frame()
  
  for (i in id) {
  
##Row binding and merging each file from all_files into the data frame data
  data <- rbind(data, read.csv(all_files[i]))
  }
  
##Calculating the mean of the pollutant and ignoring the missing values NA
  mean(data[, pollutant], na.rm = TRUE)
}

###EXAMPLE OUTPUT FOR PROBLEM 1
  
##pollutantmean("specdata", "sulfate", 1:10)
#[1] 4.064128
  
##pollutantmean("specdata", "nitrate", 70:72)
#[1] 1.706047
  
##pollutantmean("specdata", "nitrate", 23)
#[1] 1.280833
  
  
##pollutantmean("C:/Users/Lian Marie Mejos/Desktop/specdata/", "nitrate", 23)
#[1] 1.280833
  

    
#################### Problem 2 ####################

##Modifying the function pollutantmean into a function named complete
  complete <- function(directory, id = 1:332) {
    
##Creating a list of file and storing it to variable all_files
  all_files <- list.files(directory, full.names = T)
    
##Creating an empty data frame data 
  data <- data.frame()
    
  for (i in id) {
      
##Creating a variable ma and reading all_files
  ma <- read.csv( all_files[i])
      
##Calculating the sum of all complete cases in ma and storing it to variable comp
  comp <- sum(complete.cases(ma))
      
##Creating a new variable final
  final <- data.frame(i, comp)
      
##Row binding data and final
  data <- rbind(data, final)
    }
    
##Making column names "ID" and "comp"
  colnames(data) <- c("ID", "comp")
    
  data
  }

###EXAMPLE OUTPUT FOR PROBLEM 2
  
##complete("specdata", 1)
#   ID comp
#1  1  117

##complete("specdata", c(2, 4, 8, 10, 12))
#   ID comp
#1  2 1041
#2  4  474
#3  8  192
#4 10  148
#5 12   96

##complete("specdata", 30:25)
#  ID  comp
#1 30  932
#2 29  711
#3 28  475
#4 27  338
#5 26  586
#6 25  463

##complete("specdata", 3)
#   ID comp
#1  3  243
 
  
  
#################### Problem 3 ####################

##Modifying the function pollutant mean and function complete into a function named corr
  corr <- function(directory, threshold = 0){
  
##Creating a list of file and storing it to variable all_files
  all_files <- list.files(directory, full.names = T)

##Creating a numeric vector data with length 0
  data <- vector(mode = "numeric", length = 0)

##Looping each file from all_files
  for (i in 1:length(all_files)) {
    
##Reading the all_files
  ma <- read.csv(all_files[i])
    
##Calculating the sum of sulfate and nitrate
  compsum <- sum((!is.na(ma$sulfate)) & (!is.na(ma$nitrate)))
    if (compsum > threshold) {
      
##Storing available values of ma for pollutant-sulfate
  slft <- ma[which(!is.na(ma$sulfate)), ]
      
##Storing available values of slft for pollutant-nitrate
  sub <- slft[which(!is.na(slft$nitrate)), ]
      
##Calculating the correlation between sulfate and nitrate 
  data <- c(data, cor(sub$sulfate, sub$nitrate))
    }
  }
  
  data
}
  
###EXAMPLE OUTPUT FOR PROBLEM 3
  
##cr <- corr("specdata", 150)
##head(cr); summary(cr)
  
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 
  
##cr <- corr("specdata", 400)
##head(cr); summary(cr)
  
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 
  
##cr <- corr("specdata", 5000)
##head(cr); summary(cr); length(cr)
  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#[1] 0
  
##cr <- corr("specdata")
##head(cr); summary(cr); length(cr)
  
#[1] -0.22255256 -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 
#[1] 323
  
  
  
#################### Problem 4 ####################

##Setting the working directory
  setwd("C:/Users/Lian Marie Mejos/Desktop")

##Reading "outcome-of-care-measures.csv" file and storing it to variable outcome
  outcome<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  head(outcome)
  
##Returning a numeric value
  outcome[, 11]=as.numeric (outcome[, 11])
  
##Generating histogram
#Modified code
  hist(as.numeric(outcome[, 11]) , xlab= "30-day death rates from heart attack",
       main ="Histogram of the 30-day death rates from heart attack",
       col='skyblue',border='white')
  
