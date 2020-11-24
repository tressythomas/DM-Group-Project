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
library(class)


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
atl_data$MONTH=as.factor(atl_data$MONTH) 
atl_data$DAY_OF_MONTH =as.factor(atl_data$DAY_OF_MONTH)
atl_data$DAY_OF_WEEK=as.factor(atl_data$DAY_OF_MONTH)
#Create the arrival delay indicator
atl_data$arr_delay_ind=cut(atl_data$ARR_DELAY_GROUP, c(-Inf,0,Inf), c(0, 1))  #1 indicates delay
atl_data$dep_delay_ind=cut(atl_data$DEP_DELAY_GROUP, c(-Inf,0,Inf), c(0, 1))  #1 indicates delay
atl_data$delay_ind=as.factor(as.numeric(atl_data$dep_delay_ind==1 | atl_data$arr_delay_ind==1))
freq(atl_data$delay_ind)
# #Flight operation based on month
# temp=atl_data %>% 
#   group_by(MONTH,delay_ind) %>%
#   summarise(cnt=n())  
# month_stat = temp%>%
#   group_by(MONTH)%>%
#   summarise(delay_ind=delay_ind,cnt=cnt,cntx=paste0(round(cnt*100/sum(cnt),2),'%'))
# 
#  
# ggplot(data=month_stat,aes(x=as.factor(MONTH),y=cnt,fill=delay_ind,label=cntx))+
#   geom_bar(stat="identity")+
#   ggtitle("Flight operations by month") +
#   xlab("Month")+ylab("No:of Flights")+
#   geom_text(size = 3, position = position_stack(vjust = 0.5))
#   theme_minimal()+
#   scale_x_descrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
#                    labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
# 
# #Flight operation based on day of month
# ggplot(data=atl_data,aes(x=as.factor(DAY_OF_MONTH),fill=delay_ind))+
#   geom_bar(stat="count")+
#   theme_minimal()+
#   ggtitle("Flight operations by day month") +
#   xlab("Day")+ylab("No:of Flights")
# #Flight operation based on day of week
# ggplot(data=atl_data,aes(x=as.factor(DAY_OF_WEEK),fill=delay_ind))+geom_bar(stat="count")+
#   theme_minimal()+
#   ggtitle("Flight operations by Day of Week") +
#   xlab("Day")+ylab("No:of Flights")
# #Flight operation based on departure time
# temp=atl_data %>% 
#   group_by(CRS_DEP_TIME,delay_ind) %>%
#   summarise(cnt=n())  
# time_stat= temp%>%
#   group_by(CRS_DEP_TIME)%>%
#   summarise(delay_ind=delay_ind,cnt=cnt,cntx=paste0(round(cnt*100/sum(cnt),2),'%'))
# 
# ggplot(data=time_stat,aes(x=CRS_DEP_TIME ,y=cnt))+geom_line(aes(color=delay_ind))+
#   theme_minimal()
# #Flight operation based on  each airline
# temp=atl_data %>% 
#   group_by(OP_CARRIER_AIRLINE_ID,delay_ind) %>%
#   summarise(cnt=n())
# time_stat= temp%>%
#   group_by(OP_CARRIER_AIRLINE_ID)%>%
#   summarise(delay_ind=delay_ind,cnt=cnt,cntx=paste0(round(cnt*100/sum(cnt),2),'%'))
# ggplot(data=time_stat,aes(x=as.factor(OP_CARRIER_AIRLINE_ID),y=cnt,fill=delay_ind))+
#   geom_bar(stat="identity")+
#   ggtitle("Flight operations by Airline Operator") +
#   xlab("Month")+ylab("No:of Flights")+
#   theme_minimal()
#    
# #Relation between departure and arrival time delay
# ggplot(data=atl_data,aes(x=DEP_DELAY_GROUP ,y=CRS_DEP_TIME))+geom_point(aes(color=delay_ind))+
#   theme_minimal()
# 
# #Arrival delay and distance 
# ggplot(data=atl_data,aes(x=DISTANCE ,y=ARR_DELAY_GROUP))+geom_point(aes(color=delay_ind))+
#   theme_minimal()

## Most Delyayed Routes 
# 
temp=atl_data %>%
  group_by(ORIGIN,DEST,delay_ind) %>%
  summarise(cnt=n())
ROUTE_stat = temp%>%
  group_by(ORIGIN,DEST)%>%
  summarise(delay_ind=delay_ind,cnt=cnt,cntx=round(cnt*100/sum(cnt),2))

ROUTE_stat_delayed_ATL_orgn=ROUTE_stat[ROUTE_stat$delay_ind=='1'&ROUTE_stat$ORIGIN=='ATL',]
ROUTE_stat_delayed_ATL_dest=ROUTE_stat[ROUTE_stat$delay_ind=='1'&ROUTE_stat$DEST=='ATL',]

arrange(ROUTE_stat_delayed_ATL_orgn,desc(cntx))
arrange(ROUTE_stat_delayed_ATL_dest,desc(cntx,cnt))

#Most operated route

temp=atl_data %>%
  group_by(ORIGIN,DEST) %>%
  summarise(cnt=n())
ROUTE_stat = temp%>%
  group_by(ORIGIN,DEST)%>%
  summarise(cnt=cnt)
ROUTE_stat_ATL_orgn=ROUTE_stat[ROUTE_stat$ORIGIN=='ATL',]
ROUTE_stat_ATL_dest=ROUTE_stat[ROUTE_stat$DEST=='ATL',]

arrange(ROUTE_stat_ATL_orgn,desc(cnt))
arrange(ROUTE_stat_ATL_dest,desc(cnt))

#Routes withoud delay
temp=atl_data %>%
  group_by(ORIGIN,DEST,delay_ind) %>%
  summarise(cnt=n())
ROUTE_stat = temp%>%
  group_by(ORIGIN,DEST)%>%
  summarise(delay_ind=delay_ind,cnt=cnt,cntx=round(cnt*100/sum(cnt),2))

ROUTE_stat_ontime_ATL_orgn=ROUTE_stat[ROUTE_stat$delay_ind=='0'&ROUTE_stat$ORIGIN=='ATL',]
ROUTE_stat_ontime_ATL_dest=ROUTE_stat[ROUTE_stat$delay_ind=='0'&ROUTE_stat$DEST=='ATL',]

arrange(ROUTE_stat_ontime_ATL_orgn,desc(cntx))
arrange(ROUTE_stat_ontime_ATL_dest,desc(cntx,cnt))

#To do Balancing data, split, modeling-svm, logit , evaluation
options(scipen=999)
# Final dataset
fnl_data=atl_data[,-c(1,9,11,13,14)]
rm(georgia,atl_data)
set.seed(9)
# sub_ix=sample(nrow(fnl_data),20000,replace = F)
# sub_data=fnl_data[sub_ix,]
# freq(sub_data$delay_ind)
#Test train split
train_ix=createDataPartition(fnl_data$delay_ind,p=0.7, list = F)
train_data=fnl_data[train_ix,]
test_data=fnl_data[-train_ix,]
train_data_ds=downSample(x=train_data[,-c(10)],
                         y=train_data[,c(10)])


### kNN Model
#Convert the factor variables to numeric
set.seed(9)
fnl_data$MONTH=as.numeric(fnl_data$MONTH)
fnl_data$DAY_OF_MONTH=as.numeric(fnl_data$DAY_OF_MONTH)
fnl_data$DAY_OF_WEEK=as.numeric(fnl_data$DAY_OF_WEEK)
fnl_data$OP_CARRIER_AIRLINE_ID=as.numeric(fnl_data$OP_CARRIER_AIRLINE_ID)
fnl_data$ORIGIN=as.numeric(as.factor(fnl_data$ORIGIN))
fnl_data$DEST=as.numeric(as.factor(fnl_data$DEST))

train_ix=createDataPartition(fnl_data$delay_ind,p=0.7, list = F)
train_data=fnl_data[train_ix,]
test_data=fnl_data[-train_ix,]
train_data_ds=downSample(x=train_data[,-c(10)],
                         y=train_data[,c(10)])
dim(train_data)
Cls=train_data[,10]
trn=train_data[,1:9]
tst=test_data[,1:9]
scale=preProcess(trn, method = c("center", "scale"))
scaled.train=predict(scale,trn)
scaled.tst=predict(scale,tst)
knn.pred.test_y=knn(train=scaled.train,test=scaled.tst,cl=Cls,k=5)

#Test

knn_CM=confusionMatrix(knn.pred.test_y,test_data[,10])

library(xgboost)
# trn=as.matrix(train_data_ds[,1:9])
# Cls=as.matrix(train_data_ds[,10])
trn=as.matrix(train_data[,1:9])
Cls=train_data[,10]
Cls=as.matrix(Cls)
tst=as.matrix(test_data[,1:9])

### gbm Model
xgb.fit <- xgboost(
  data = trn,
  label = Cls,
  nrounds = 500,
  objective ="binary:logistic",
  #nfold = 5,
  verbose = 0               # evaluation metric out,
)
# plot error vs number trees
# ggplot(xgb.fit$evaluation_log) +
#   geom_line(aes(iter, train_rmse_mean), color = "red") +
#   geom_line(aes(iter, test_rmse_mean), color = "blue")

prob.pred_test_y = predict(xgb.fit, tst)
xgb.pred_test_y <- ifelse(prob.pred_test_y > 0.5, 1, 0)
xgb_CM=confusionMatrix(factor(xgb.pred_test_y),test_data[,10])
# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 5, measure = "Gain")


# Neural Network 
fnl_data$delay_ind=as.numeric(fnl_data$delay_ind)
maxs <- apply(fnl_data, 2, max) 
mins <- apply(fnl_data, 2, min)
scaled <- as.data.frame(scale(fnl_data, center = mins, scale = maxs - mins))
train_data=scaled[train_ix,]
test_data=scaled[-train_ix,]

library(neuralnet)
n <- names(train_data)
f <- as.formula(paste("delay_ind ~", paste(n[!n %in% "delay_ind"], collapse = " + ")))
nn <- neuralnet(f,data=train_data,hidden=c(8,4),linear.output=F)


dim(train_data_ds)
logit=glm(Class~.,train_data_ds,family="binomial")
logit.model=train(delay_ind~.,
                  data=train_data,
                  #preProcess = c("center","scale"),
                  method = "glm",family="binomial"
)
summary(logit.model)
#Test



# # logit_all=glm(delay_ind~.,train_data,family="binomial")
# # summary(logit_all)
# ######## Train Accuracy
# train_pred=predict(logit,train_data_ds[,1:9],type = "response")
# # Recode factors
# train_pred <- ifelse(train_pred > 0.5, 1, 0)
# mean(train_data_ds$Class==train_pred)
# 
test_pred=predict(logit,test_data[,1:9],type = "response")
test_pred =ifelse(test_pred > 0.5, 1, 0)
mean(test_data$delay_ind==test_pred)
test_true=as.factor(test_data$delay_ind)
Logit_CM=confusionMatrix(factor(test_pred), test_true)
