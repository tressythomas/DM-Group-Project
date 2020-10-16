library(readr)
library(VIM)
library(finalfit)
library(naniar)

# Get all files from the folder
filenames=list.files(path="C:/Users/Tressy/Desktop/Semester 3/Data Mining/Group Project/DM Project Georgia Ontime 2019",pattern="*.csv",full.names = T)
georgia=do.call(rbind,lapply(filenames,function(x) read.csv(x, stringsAsFactors = FALSE, header=TRUE, sep=',', na.strings=c("","N/A"," ","NA",'NULL'))))
dim(georgia)
#Get the flights to and from ATL. And remove the last column. empty column from import
atl.ix=which(georgia$ORIGIN=='ATL'|georgia$DEST=='ATL')
atl_data=georgia[atl.ix,-27]
str(atl_data)
check_miss(atl_data)

check_miss<-function(atl_data){
  
  cat("Original Dataset dimension:" ,dim(atl_data))
  cat("Total missing values: ", sum(is.na(atl_data)))      #check for missing values
  col_miss=apply(atl_data,2,function(x) sum(is.na(x)))   
  print(col_miss)                                          #check for missing values for each column
  
  #missing_plot(atl_data)                                   #Visualize missingness to check for any patterns
  #vis_miss(atl_data,warn_large_data=FALSE)
  missing_pattern(atl_data)
  # Looks like the issing values are for "cancelled flights". We are not considereing "cancelled" flights in this analysis.
  # Remove the "cancelled ==1" instances.
  atl_data=atl_data[atl_data$CANCELLED==0 & atl_data$DIVERTED==0,]
  cat("Total missing values after removing cancelled, diverted flights: ", sum(is.na(atl_data)))
  cat("Dataset dimension after missing value treatment :", dim(atl_data))
}

