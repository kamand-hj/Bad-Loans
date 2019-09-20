### risk etebar

tr1 <- read.csv(file.choose())
ts1 <- read.csv(file.choose())


summary(tr1)
tr1$pymnt_plan<- as.factor(tr1$pymnt_plan)
tr1$home_ownership<-as.factor(tr1$home_ownership)
tr1$purpose <- as.factor(tr1$purpose)

tr2 <- tr1

#### duplicate

sum(duplicated(tr1))


#### Miss & Outlier

colMeans(is.na(tr1))*100


# Outlier

library(DMwR)

sum(!complete.cases(tr1))

missRow <- which(!complete.cases(tr1))

for(i in 1:ncol(tr1)){
  tr1[,i] <- as.numeric(tr1[,i])
}

missData <- tr1[missRow,]
notMissData <- tr1[-missRow,]

notMissData2 <- notMissData[,-21]
notMissData2 <- as.matrix(notMissData2)

lof <- lofactor(data = notMissData2, k=5)

notMissData2 <- data.frame(notMissData2)
notMissData2$lof <- lof

hist(notMissData2$lof, breaks = 40)

sum(notMissData2$lof > 1.5)

notMissData2 <- cbind(notMissData2[,-21],notMissData[,21],notMissData2[,21])
names(notMissData2)[c(21,22)] <- c("bad_loans","lof")

notMissData2 <- notMissData2[notMissData2$lof < 1.5,]

notMissData2 <- notMissData2[,-22]

tr2 <- rbind(notMissData2,missData)

##### Missing

colMeans(is.na(tr2))*100

tr2$payment_inc_ratio[is.na(tr2$payment_inc_ratio)] <- round(mean(tr2$payment_inc_ratio,na.rm = TRUE))
tr2$delinq_2yrs_zero[is.na(tr2$delinq_2yrs_zero)] <- round(mean(tr2$delinq_2yrs_zero,na.rm = TRUE))
tr2$inq_last_6mths[is.na(tr2$inq_last_6mths)] <- round(mean(tr2$inq_last_6mths,na.rm = TRUE))
tr2$open_acc[is.na(tr2$open_acc)] <- round(mean(tr2$open_acc,na.rm = TRUE))
tr2$pub_rec[is.na(tr2$pub_rec)] <- round(mean(tr2$pub_rec,na.rm = TRUE))
tr2$pub_rec_zero[is.na(tr2$pub_rec_zero)] <- round(mean(tr2$pub_rec_zero,na.rm = TRUE))



##balancing
library(DMwR)
table(tr2$bad_loans)
tr2$bad_loans <- as.factor(tr2$bad_loans)
tr3 <- SMOTE(bad_loans ~ ., tr2, perc.over = 100,
             k = 5, perc.under =50)
table(tr3$bad_loans)
tr4 <- tr3

##Modeling
set.seed(13579)
sm1 <- sample(1:nrow(tr3), nrow(tr3) / 3, replace = FALSE)
test1 <- tr3[sm1,]
train1 <- tr3[-sm1,]


###dt
library(rpart)
library(rpart.plot)

fit.dt <- rpart(bad_loans~. , data = train1, method = "class")

rpart.plot(fit.dt,type = 3, extra = 2,cex=0.6)

pred.dt <- predict(fit.dt,test1)

p <- ifelse(pred.dt[,2]>0.5,1,0)

table(test1$bad_loans, p)
mean(test1$bad_loans == p)

library(ROSE)

accuracy.meas(test1$bad_loans,pred.dt[,2])


###### KNN
library(caret)
train2 <- na.omit(train1)
test2 <- na.omit(test1)

knnmd <- knn3(bad_loans~. ,train2, k=19)
pred.knn <- predict(knnmd, test2, type = "class")
table(test2$bad_loans , pred.knn)
mean(test2$bad_loans == pred.knn)
accuracy.meas(test2$bad_loans,pred.knn)

library(e1071)
tr3 <- na.omit(tr2)
x <- tr3[,-21]
y <- tr3[,21]

tune1 <- tune.knn(x,y, k= 10:20,
                  tunecontrol=tune.control(sampling = "fix"))
tune1$best.parameters



##### Naive Bayes

library(e1071)

train2 <- na.omit(train1)
test2 <- na.omit(test1)

nb1 <- naiveBayes(bad_loans~. , train2)
pred.NB <- predict(nb1,test2, type = "class")

table(test2$bad_loans,pred.NB)
mean(test2$bad_loans == pred.NB)
accuracy.meas(test2$bad_loans,pred.NB)


nb2 <- naiveBayes(bad_loans~. , train2, laplace = 0.0001)
pred.NB2 <- predict(nb2,test2, type = "class")

table(test2$bad_loans,pred.NB2)
mean(test2$bad_loans == pred.NB2)
accuracy.meas(test2$bad_loans,pred.NB2)


# Bagging

require(ipred)
require(caret)
require(party)


bag1 <- bagging(bad_loans~. , data = train1 , nbagg=15)

pred.BAG <- predict(bag1, test1, type = "class")

table(test1$bad_loans , pred.BAG)
mean(test1$bad_loans == pred.BAG)

predicted <- as.factor(sample(c(0,1),100,replace= T))
realized <- as.factor(sample(c(0,1),100,replace= T))

result <- confusionMatrix(predicted,realized,mode = "prec_recall")

result
result$byClass["F1"]

accuracy.meas(test1$bad_loans,result)



###### Boosting

library(adabag)

boostfit1 <- boosting(bad_loans~. , train1, mfinal = 20)

pred1 <- predict(boostfit1, test1)

pred1$confusion
pred1$error
pred1$class
mean(test1$bad_loans == pred1$class)



boosfit2 <- boosting(bad_loans~. , data = train1,
                     mfinal = 50,
                     control = rpart.control(maxdepth = 3))

pred2 <- predict(boosfit2,test1)

mean(test1$bad_loans == pred2$class)

pred1$confusion
pred1$error
pred1$class
mean(test1$bad_loans == pred1$class)


boosfit3 <- boosting(bad_loans~. , data = train1,
                     mfinal = 50, coeflearn = "Zhu",
                     control = rpart.control(maxdepth = 3))


pred3 <- predict(boosfit3,test1)

mean(test1$bad_loans == pred3$class)
pred1$confusion
pred1$error
pred1$class
mean(test1$bad_loans == pred1$class)



######
summary(ts1)
ts1$pymnt_plan<- as.numeric(ts1$pymnt_plan)
ts1$home_ownership<-as.numeric(ts1$home_ownership)
ts1$purpose <- as.numeric(ts1$purpose)
ts1$grade <- as.numeric(ts1$purpose)


##### Missing

colMeans(is.na(ts1))*100

ts1$payment_inc_ratio[is.na(ts1$payment_inc_ratio)] <- round(mean(ts1$payment_inc_ratio,na.rm = TRUE))
ts1$delinq_2yrs[is.na(ts1$delinq_2yrs)] <- round(mean(ts1$delinq_2yrs,na.rm = TRUE))
ts1$delinq_2yrs_zero[is.na(ts1$delinq_2yrs_zero)] <- round(mean(ts1$delinq_2yrs_zero,na.rm = TRUE))
ts1$inq_last_6mths[is.na(ts1$inq_last_6mths)] <- round(mean(ts1$inq_last_6mths,na.rm = TRUE))
ts1$open_acc[is.na(ts1$open_acc)] <- round(mean(ts1$open_acc,na.rm = TRUE))
ts1$pub_rec[is.na(ts1$pub_rec)] <- round(mean(ts1$pub_rec,na.rm = TRUE))
ts1$pub_rec_zero[is.na(ts1$pub_rec_zero)] <- round(mean(ts1$pub_rec_zero,na.rm = TRUE))


#####

res1 <- predict(knnmd,ts1,type = "class")
res1 <- predict(boostfit1,ts1)
res1
res2<- as.factor(res1)

write.table(res1,"E:\\R\\result3.csv",col.names = FALSE,row.names = FALSE)






