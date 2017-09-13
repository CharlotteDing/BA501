##Find my Precision_recall curve ppt here,
##https://drive.google.com/file/d/0B-jAfwawYf76bldzT0JmVHNDc3c/view?usp=sharing

#Load packages
library(data.table)
library(dplyr)
library(caret)
library(mice)
library(glmnet)
library(caTools)
library(ROCR)

#Read datasets
customer<- fread("D:/customer_table.csv")
orders<- fread("D:/order_table.csv")
product<- fread("D:/product_table.csv")
category<- fread("D:/category_table.csv")

#Select customers who made only one purchase before 2016/12/22;
onepurchase<-orders %>%
        filter(order_date<20161222,order_amount>0) %>%
        group_by(customer_id) %>%
        mutate(count=n()) %>%
        filter(count==1)
        
#Select users go dormant for 3 months in onepurchase;
is_buyer<-orders %>%
        filter(order_date>=20161222,order_date<20170222,order_amount>0) %>%
        group_by(customer_id) %>%
        summarise(sum(order_amount))
dormant<-onepurchase[!(onepurchase$customer_id %in% is_buyer$customer_id),]

#Flag users who come back
isback<-orders %>%
        filter(order_date>=20170222,order_amount>0) %>%
        group_by(customer_id) %>%
        summarise(n())

dormant$back<-ifelse(dormant$customer_id %in% isback$customer_id,1,0)

#Fill in features
dormant<-dormant[,c(1,3,4,7),with=F]
sample<-inner_join(dormant,customer,by="customer_id")
index<-duplicated(sample$customer_id)
sample<-sample[!index,]

#Sanity check,deal with characters,dates
summary(sample)
str(sample)
sample$country<-as.numeric(as.factor(sample$country))
sample$gender<-as.numeric(as.factor(sample$gender))
sample<-sample[,-c(1,11,12),with=F]
cor(sample)

#Skewness check example
qqnorm(sample$user_feature1)

#Deal with missing values,dummy & impute
sample$non_latest_device<-ifelse(is.na(sample$latest_device_class),1,0)
sample$age_c<-cut(sample$age,breaks=10,labels=F,include.lowest = T,right=F)

#out of RAM for imputation
##temp<-mice(sample,method="pmm",maxit=5,seed=88)
##data <- complete(temp,1)

#The NA % in rest features is minor, so it's fine to delete some observations here
data<-sample[,-c(10,13),with=F]
data<-data[complete.cases(data),]

#Scale, delete features with all 0
data<-cbind(scale(data[,-c(3,5:7,9,31,61,91,112),with=F]),data[,c(3,5:7,9,112),with=F])

#Set up datasets
set.seed(88)
split<-createDataPartition(data$back,p=0.6,list=F)
train<-data[split,]
test<-data[-split,]

#lasso
train_x <- as.matrix(train[,-105,with=F])
train_y <- as.factor(ifelse(train$back==1,'YES', 'NO'))
lasso<-cv.glmnet(train_x,train_y,family="binomial",type.measure = "auc")
plot(lasso)

#Evaluation
test_x<-as.matrix(test[,-105,with=F])

predictTest<-predict(lasso,test_x,s="lambda.min",type="response")
ROCRpred<-prediction(predictTest,test$back)

par(mfrow = c(1,2)) 

ROCRperf<-performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,0.2,by=0.05), text.adj=c(-0.2,1.7),main="ROC curve with threshold labels")

PRperf<-performance(ROCRpred, "prec", "rec")
plot(PRperf, colorize=TRUE, print.cutoffs.at=seq(0,0.2,by=0.05), text.adj=c(-0.2,1.7),main="PR curve with threshold labels")
