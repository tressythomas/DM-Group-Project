library(readr)
library(VIM)
library(finalfit)
library(naniar)
library(ggplot2)
library(funModeling)
library(GGally)
library(dplyr)

# Get all files from the folder
filenames=list.files(path="C:/Users/Tressy/Desktop/Semester 3/Data Mining/Group Project/DM Project Georgia Ontime 2019",pattern="*.csv",full.names = T)
# filenames=list.files(path="https://github.com/tressythomas/DM-Group-Project/upload/main/Datasets", pattern="*.csv",full.names = T)
# filenames=read.csv("https://github.com/tressythomas/DM-Group-Project/upload/main/Datasets/41792460_T_ONTIME_REPORTING.csv")

georgia=do.call(rbind,lapply(filenames,function(x) read.csv(x, stringsAsFactors = FALSE, header=TRUE, sep=',', na.strings=c("","N/A"," ","NA",'NULL'))))
dim(georgia)
#Get the flights to and from ATL. And remove the last column. empty column from import
atl.ix=which(georgia$ORIGIN=='ATL'|georgia$DEST=='ATL')
atl_data=na.omit(georgia[atl.ix,-c(12,14)])
str(atl_data)
dim(atl_data)
#atl_data=check_miss(atl_data)
#atl_data=atl_data[,-c(24,25)]
#Remove unwanted columns. FL_DATE, ORIGIN_AIRPORT_ID,ORIGIN_CITY_NAME,ORIGIN,DEST_AIRPORT_ID,DEST,DEST_CITY_NAME,DEP_TIME,DEP_DELAY_NEW,DEP_DELAY,DEP_DELAY_NEW,ARR_TIME,ARR_DELAY,ARR_DELAY_NEW 
#Convert the datatypes appropriately - to factor- OP_CARRIER_AIRLINE_ID
unique(atl_data$OP_CARRIER_AIRLINE_ID) #15 different airlines operating to and from ATL
atl_data$OP_CARRIER_AIRLINE_ID=as.factor(atl_data$OP_CARRIER_AIRLINE_ID)
#Create the arrival delay indicator
atl_data$delay_ind=cut(atl_data$ARR_DELAY_GROUP, c(-Inf,0,Inf), c(0, 1))  #1 indicates delay
freq(atl_data$delay_ind)
#Flight operation based on month
month_stat=atl_data %>% 
  group_by(MONTH,delay_ind) %>%
  summarise(cnt=n())   # select count * , from atl_data group by (MONTH,delay_ind )

ggplot(data=month_stat,aes(x=as.factor(MONTH),y=cnt,fill=delay_ind))+geom_bar(stat="identity")+
  theme_minimal()+
  scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
#Flight operation based on day of month
ggplot(data=atl_data,aes(x=as.factor(DAY_OF_MONTH),fill=delay_ind))+geom_bar(stat="count")+
  theme_minimal()
#Flight operation based on day of week
ggplot(data=atl_data,aes(x=as.factor(DAY_OF_WEEK),fill=delay_ind))+geom_bar(stat="count")+
  theme_minimal()
#Flight operation based on departure time
time_stat=atl_data %>% 
  group_by(CRS_DEP_TIME,delay_ind) %>%
  summarise(cnt=n())
ggplot(data=time_stat,aes(x=CRS_DEP_TIME ,y=cnt))+geom_line(aes(color=delay_ind))+
  theme_minimal()
#Flight operation based on arrival time of each airline
time_stat=atl_data %>% 
  group_by(CRS_ARR_TIME,OP_CARRIER_AIRLINE_ID,delay_ind) %>%
  summarise(cnt=n())
ggplot(data=time_stat,aes(x=CRS_ARR_TIME ,y=cnt))+geom_line(aes(color=OP_CARRIER_AIRLINE_ID,linetype=delay_ind))+
  theme_minimal()
#Relation between departure and arrival time delay
ggplot(data=atl_data,aes(x=DEP_DELAY_GROUP ,y=ARR_DELAY_GROUP))+geom_point(aes(color=delay_ind))+
  theme_minimal()
#Arrival delay for operators
operator_stat=atl_data %>% 
  group_by(OP_CARRIER_AIRLINE_ID,delay_ind) %>%
  summarise(cnt=n())
temp_sum=operator_stat %>% 
   group_by(OP_CARRIER_AIRLINE_ID) %>%
   summarise(sum=sum(cnt))
operator_stat$pct=operator_stat$cnt/(temp_sum$sum[])
ggplot(data=operator_stat,aes(x=OP_CARRIER_AIRLINE_ID,y=cnt))+geom_bar(stat="identity",aes(fill=delay_ind))
#Arrival delay and distance 
ggplot(data=atl_data,aes(x=DISTANCE ,y=ARR_DELAY_GROUP))+geom_point(aes(color=delay_ind))+
  theme_minimal()

#To do Balancing data, split, modeling-svm, logit , evaluation


freq(atl_data$ARR_DELAY_GROUP)
atl_data$delay_ind=delay_ind
hist(atl_data$DAY_OF_WEEK)
plot(atl_data$DEP_DELAY,atl_data$DEP_TIME)

ggplot(data=atl_data,aes(x=DEP_DELAY,y=DEP_TIME,fill=delay_ind))+geom_point(position="identity")+theme_minimal()

ggplot(data=atl_data,aes(x=DAY_OF_WEEK,fill=delay_ind))+geom_bar(stat="count")+theme_minimal()

#Check distribution between time of the dayoftheweek vs delay
#time vs delay
#distance vs delay
ggplot(data=atl_data,aes(x=DISTANCE,y=ARR_DELAY_GROUP,color=delay_ind))+geom_point(stat="identity")+theme_minimal()
#airline OP_CARRIER_AIRLINE_ID vs delay
#most common delays for  ARR_DELAY_GROUP,  DEP_DELAY_GROUP
ggplot(data=atl_data,aes(x=ARR_DELAY_GROUP,fill=delay_ind))+geom_bar(stat="count")+theme_minimal()
ggplot(data=atl_data,aes(x=DEP_DELAY_GROUP,fill=delay_ind))+geom_bar(stat="count")+theme_minimal()
#check outliers, data distribution, feature extraction, EDA,  training/test split 


check_miss<-function(atl_data){
  
  cat("Original Dataset dimension:" ,dim(atl_data))
  cat("Total missing values: ", sum(is.na(atl_data)))      #check for missing values
  col_miss=apply(atl_data,2,function(x) sum(is.na(x)))   
  print(col_miss)                                          #check for missing values for each column
  
  #missing_plot(atl_data)                                  #Visualize missingness to check for any patterns
  #vis_miss(atl_data,warn_large_data=FALSE)
  missing_pattern(atl_data)
  # Looks like the issing values are for "cancelled flights". We are not considereing "cancelled" flights in this analysis.
  # Remove the "cancelled ==1" instances.
  atl_data=atl_data[atl_data$CANCELLED==0 & atl_data$DIVERTED==0,]
  cat("Total missing values after removing cancelled, diverted flights: ", sum(is.na(atl_data)))
  cat("Dataset dimension after missing value treatment :", dim(atl_data))
  return(atl_data)
}

