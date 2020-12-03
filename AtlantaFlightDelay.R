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
georgia=do.call(rbind,lapply(filenames,function(x) read.csv(x, stringsAsFactors = FALSE, header=TRUE, sep=',', na.strings=c("","N/A"," ","NA",'NULL'))))
dim(georgia)

#Get the flights to and from ATL. And remove the last column. empty column from import
atl.ix=which(georgia$ORIGIN=='ATL'|georgia$DEST=='ATL')
atl_data=na.omit(georgia[atl.ix,-c(12,14)])
dim(atl_data)

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
##############################Exploratory Data Analysis#######################################################

###Flight operation based on month
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

## Most Delyayed Routes 
  
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

#Routes without delay
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


#Generate maps with most delays origins and destinations

# flat=c(33.6407,33.6407,33.6407,33.6407,33.6407,33.6407,33.6407,33.6407,33.6407,33.6407)
# flon=c(-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277)
# Generate maps showing origin and destination with most delays
# 
# tlat=c(40.7904,40.6895,61.1759,40.2770,37.8044,37.7749,44.0805,38.8339,40.7128,34.0522)
# tlon=c(-73.1001,-74.1745,-149.9901,-74.8181,,-122.2712,-122.4194,-103.231,-104.8214,-74.006,-118.2437)
# 
# 
tlat=c(33.6407,33.6407,33.6407,33.6407,33.6407,33.6407,33.6407,33.6407,33.6407,33.6407)
tlon=c(-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277,-84.4277)
flat=c(46.8721
       ,39.5296
       ,33.8303
       ,46.8772
       ,61.1759
       ,34.1808
       ,40.7298
       ,39.1911
       ,17.7466
       ,44.0805
)
flon=c(-113.994
        ,-119.8138
        ,-116.5453
        ,-96.7898
        ,-149.9901
        ,-118.309
        ,-73.2104
        ,-106.8175
        ,-64.7032
        ,-103.231
)
delay_map=as.data.frame(cbind(flat,flon,tlat,tlon))
# Plot route
library(maps)
# No margin
par(mar=c(0,0,0,0))
# US map
usMap = borders("world", colour='grey', fill="ivory")
allUSA <- ggplot()  + usMap +
  geom_curve(data=delay_map,
             aes(x=flon, y=flat, xend=tlon, yend=tlat),
             col="cyan4",
             arrow = arrow(length = unit(0.25,"cm")),
             size=1.0,
             ncp = 500,
             curvature=0.4) +
  geom_point(data=delay_map,
             aes(x=flon, y=flat), 
             colour="orange",
             size=2.5) +
    geom_point(data=delay_map,
             aes(x=tlon, y=tlat),
             size=5.5,
             colour="mediumorchid4") +
  geom_text_repel(data=delay_map,position = 'identity', aes(x = flon, y = flat, 
                                                            label = c('Missoula(1)','Reno(2)','Palp Springs(3)','Fargo(4)', 'Anchorage(5)','Burbank(6)','Islip(7)','Aspen(8)','Christianstek(9)','Rapid City(10)')),
                  #label = c('Islip(1)','Newark(2)','Archonage(3)','Trenton(4)', 'Oackland(5)','San Francisco(6)','Rapid City(7)','Colorado Springs(8)','New York(9)','Los Angeles(10)')),
                  col = "black", size =3) + 
  geom_text(data=delay_map,position = 'identity', aes(x = tlon[1], y = tlat[1], label = c('Atlanta')), col = "black", size =5)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title=element_text(hjust=0.5, size=14)) +
  coord_cartesian(ylim=c(16.5, 62.5), xlim=c(-152, -62)) +
  ggtitle("Top Ten Origin-locations of Flight  delay towards Atlanta")
allUSA


##################################To do Balancing data #################################
options(scipen=999)
# Final dataset
fnl_data=atl_data[,-c(1,9,11,13,14)]
rm(georgia,atl_data)
set.seed(9)
#Test train split
train_ix=createDataPartition(fnl_data$delay_ind,p=0.7, list = F)
train_data=fnl_data[train_ix,]
test_data=fnl_data[-train_ix,]

################################# undersampling of training data #################################
train_data_ds=downSample(x=train_data[,-c(10)],
                         y=train_data[,c(10)])


#################################### kNN Model #################################
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
#Scale the dataset 
scale=preProcess(trn, method = c("center", "scale"))
scaled.train=predict(scale,trn)
scaled.tst=predict(scale,tst)
knn.pred.test_y=knn(train=scaled.train,test=scaled.tst,cl=Cls,k=5)

#Test

knn_CM=confusionMatrix(knn.pred.test_y,test_data[,10])




################################# xgbm Model ##############################
library(xgboost)

trn=as.matrix(train_data[,1:9])
Cls=train_data[,10]
Cls=as.matrix(Cls)
tst=as.matrix(test_data[,1:9])
# create parameter list
# hyper_grid <- expand.grid(
#   eta = c(.01, .05, .1, .3),
#   max_depth = c(1, 3),
#   min_child_weight = c(1, 3),
#   subsample = c(.65, .8), 
#   colsample_bytree = c(.8,),
#   optimal_trees = 0,               # a place to dump results
# )

xgb.fit <- xgboost(
  data = trn,
  label = Cls,
  nrounds = 1000,
  objective ="binary:logistic",
  #nfold = 5,
  early_stopping_rounds = 20,
  verbose = 0               # evaluation metric out,
)
 #  plot error vs number trees
 # ggplot(xgb.fit$evaluation_log) +
 #   geom_line(aes(iter, train_rmse_mean), color = "red") +
 #  geom_line(aes(iter, test_rmse_mean), color = "blue")

prob.pred_test_y = predict(xgb.fit, tst)
xgb.pred_test_y <- ifelse(prob.pred_test_y > 0.5, 1, 0)
xgb_CM=confusionMatrix(factor(xgb.pred_test_y),test_data[,10])
xgb_CM
# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 9, measure = "Gain")

############################## Logistic regression  ##############################
dim(train_data_ds)
logit=glm(Class~.,train_data_ds,family="binomial")
logit.model=train(delay_ind~.,
                  data=train_data,
                  #preProcess = c("center","scale"),
                  method = "glm",family="binomial"
)
summary(logit.model)

test_pred=predict(logit,test_data[,1:9],type = "response")
test_pred =ifelse(test_pred > 0.5, 1, 0)
mean(test_data$delay_ind==test_pred)
test_true=as.factor(test_data$delay_ind)
Logit_CM=confusionMatrix(factor(test_pred), test_true)

############################## Neural network ##############################
# library(neuralnet)
# n <- names(train_data)
# f <- as.formula(paste("delay_ind ~", paste(n[!n %in% "delay_ind"], collapse = " + ")))
# nn <- neuralnet(f,data=train_data,hidden=c(8,4),linear.output=F)
# # Neural Network 
# fnl_data$delay_ind=as.numeric(fnl_data$delay_ind)
# maxs <- apply(fnl_data, 2, max) 
# mins <- apply(fnl_data, 2, min)
# scaled <- as.data.frame(scale(fnl_data, center = mins, scale = maxs - mins))
# train_data=scaled[train_ix,]
# test_data=scaled[-train_ix,]