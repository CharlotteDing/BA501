#Load packages
library(data.table)
library(sqldf)
library(dplyr)
library(glmnet)
library(caTools)
library(ROCR)

#Read datasets
customer<- fread("D:/customer_table.csv")
orders<- fread("D:/order_table.csv")
product<- fread("D:/product_table.csv")
category<- fread("D:/category_table.csv")

#Select customers who made only one purchase before 2016/12/22;
before<-filter(orders,order_date<20161222)
purchase<-mutate(group_by(before,customer_id),count=n())
onepurchase<-filter(purchase,count==1)


#Select users go dormant for 3 months in onepurchase;
dormant<-sqldf("select b.customer_id,b.product_id,b.order_amount
               from onepurchase b
               left join orders o on o.customer_id=b.customer_id
               and o.order_date>=20161222
               and o.order_date<20170222
               where o.customer_id is NULL")


#Flag users who come back
back<-sqldf("select distinct d.customer_id,d.product_id,d.order_amount
            from dormant d
            join orders o on o.customer_id=d.customer_id
            where order_date>=20170222")
back$back<-1

notback<-sqldf("select d.customer_id,d.product_id,d.order_amount
            from dormant d
              left join orders o on o.customer_id=d.customer_id
               and order_date>=20170222
               where o.customer_id is NULL")
notback$back<-0

sample<-rbind(back,notback)

#Fill in features
sample_features<-inner_join(sample,customer,by="customer_id")

#Deal with characters
sample_features$country<-as.numeric(as.factor(sample_features$country))
sample_features$gender<-as.numeric(as.factor(sample_features$gender))
sample_features<-sample_features[,-c(1,11,12,16,117)]

#Delete NAs
data<-sample_features[complete.cases(sample_features),]

#Prepare datasets
set.seed(88)
split<-sample.split(data$back, SplitRatio = 0.6)
train<-subset(data, split == TRUE)
test<-subset(data, split == FALSE)

#logistic regression
model<-glm(back~.,family=binomial, train)
summary(model)
feaselection<-data[,c(1:3,5,6,8,9,12:17,19,20,22,23,25,39,41,50,52,55,76,77,80,82,83,85,106,107,110)]

#Evaluation
predictTrain<-predict(model, type="response")
ROCRpred<-prediction(predictTrain, train$back)
ROCRperf<-performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,0.2,by=0.01), text.adj=c(-0.2,1.7),main="ROC curve with threshold labels")

predictTest<-predict(model,newdata=test,type="response")
table(test$back,predictTest>0.15)

#lasso?
ltrain<-as.matrix(train)
lasso<-glmnet(ltrain[,-3],ltrain[,3],family = "binomial")
plot(lasso, xvar="lambda")