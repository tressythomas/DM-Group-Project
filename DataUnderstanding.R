library(readr)
library(VIM)
library(finalfit)
library(naniar)
library(ggplot2)
library(funModeling)
library(GGally)
library(ggthemes)
library(dplyr)
library(tidyverse)
library(caret)
library(e1071)

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
atl_data[,2:4]=as.factor(atl_data[,2:4]) 
#Create the arrival delay indicator
atl_data$arr_delay_ind=cut(atl_data$ARR_DELAY_GROUP, c(-Inf,0,Inf), c(0, 1))  #1 indicates delay
atl_data$dep_delay_ind=cut(atl_data$DEP_DELAY_GROUP, c(-Inf,0,Inf), c(0, 1))  #1 indicates delay
atl_data$delay_ind=as.factor(as.numeric(atl_data$dep_delay_ind==1 | atl_data$arr_delay_ind==1))
freq(atl_data$delay_ind)
#Flight operation based on month
temp=atl_data %>% 
  group_by(MONTH,delay_ind) %>%
  summarise(cnt=n())  
month_stat = temp%>%
  group_by(MONTH)%>%
  summarise(delay_ind=delay_ind,cnt=cnt,cntx=paste0(round(cnt*100/sum(cnt),2),'%'))

 
ggplot(data=month_stat,aes(x=as.factor(MONTH),y=cnt,fill=delay_ind,label=cntx))+
  geom_bar(stat="identity")+
  ggtitle("Flight operations by month") +
  xlab("Month")+ylab("No:of Flights")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))
  theme_minimal()+
  scale_x_descrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))

#Flight operation based on day of month
ggplot(data=atl_data,aes(x=as.factor(DAY_OF_MONTH),fill=delay_ind))+
  geom_bar(stat="count")+
  theme_minimal()+
  ggtitle("Flight operations by day month") +
  xlab("Day")+ylab("No:of Flights")
#Flight operation based on day of week
ggplot(data=atl_data,aes(x=as.factor(DAY_OF_WEEK),fill=delay_ind))+geom_bar(stat="count")+
  theme_minimal()+
  ggtitle("Flight operations by Day of Week") +
  xlab("Day")+ylab("No:of Flights")
#Flight operation based on departure time
temp=atl_data %>% 
  group_by(CRS_DEP_TIME,delay_ind) %>%
  summarise(cnt=n())  
time_stat= temp%>%
  group_by(CRS_DEP_TIME)%>%
  summarise(delay_ind=delay_ind,cnt=cnt,cntx=paste0(round(cnt*100/sum(cnt),2),'%'))

ggplot(data=time_stat,aes(x=CRS_DEP_TIME ,y=cnt))+geom_line(aes(color=delay_ind))+
  theme_minimal()
#Flight operation based on  each airline
temp=atl_data %>% 
  group_by(OP_CARRIER_AIRLINE_ID,delay_ind) %>%
  summarise(cnt=n())
time_stat= temp%>%
  group_by(OP_CARRIER_AIRLINE_ID)%>%
  summarise(delay_ind=delay_ind,cnt=cnt,cntx=paste0(round(cnt*100/sum(cnt),2),'%'))
ggplot(data=time_stat,aes(x=as.factor(OP_CARRIER_AIRLINE_ID),y=cnt,fill=delay_ind))+
  geom_bar(stat="identity")+
  ggtitle("Flight operations by Airline Operator") +
  xlab("Month")+ylab("No:of Flights")+
  theme_minimal()
   
#Relation between departure and arrival time delay
ggplot(data=atl_data,aes(x=DEP_DELAY_GROUP ,y=CRS_DEP_TIME))+geom_point(aes(color=delay_ind))+
  theme_minimal()

#Arrival delay and distance 
ggplot(data=atl_data,aes(x=DISTANCE ,y=ARR_DELAY_GROUP))+geom_point(aes(color=delay_ind))+
  theme_minimal()

#To do Balancing data, split, modeling-svm, logit , evaluation
options(scipen=999)
# Final dataset
fnl_data=atl_data[,-c(1,9,11,13,14)]
#Test train split
train_ix=createDataPartition(fnl_data$delay_ind,p=0.7, list = F)
train_data=fnl_data[train_ix,]
test_data=fnl_data[-train_ix,]
train_data_ds=downSample(x=train_data[,-c(10)],
                      y=train_data[,c(10)])
dim(train_data_ds)
logit=glm(Class~.,train_data_ds,family="binomial")
summary(logit)
# logit_all=glm(delay_ind~.,train_data,family="binomial")
# summary(logit_all)
######## Train Accuracy
train_pred=predict(logit,train_data_ds[,1:9],type = "response")
# Recode factors
train_pred <- ifelse(train_pred > 0.5, 1, 0)
mean(train_data_ds$Class==train_pred)

test_pred=predict(logit,test_data[,1:9],type = "response")
test_pred <- ifelse(test_pred > 0.5, 1, 0)
mean(test_data$delay_ind==test_pred)
test_true=test_data$delay_ind
confusionMatrix(factor(test_pred), test_true)
#SVM Model

svm_model=svm(Class~.,train_data_ds, kernel = "radial", gamma = 1, cost = 1)
train_pred=predict(svm_model,train_data_ds[,1:9],type = "response")
# Recode factors
train_pred <- ifelse(train_pred > 0.5, 1, 0)
mean(train_data_ds$Class==train_pred)

test_pred=predict(svm_model,test_data[,1:9],type = "response")
test_pred <- ifelse(test_pred > 0.5, 1, 0)
mean(test_data$delay_ind==test_pred)
confusionMatrix(factor(test_pred), test_data$delay_ind)

# tune model to find optimal cost, gamma values
tune.out <- tune(svm, Class~.,train_data_ds, kernel = "radial",
                 ranges = list(cost = c(0.1,1,10,100),
                               gamma = c(0.5,1,2,3,4)))
# show best model
tune.out$best.model


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

