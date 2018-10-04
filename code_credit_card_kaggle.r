#All the data will be coming as numbers instead of exponential
options(scipen=999)

#Get the directory
getwd()

#Set the directory
setwd(choose.dir())

#Important packages to be loaded from memory
library(dplyr)

#Importing the dataset
credit_card<- read.csv("D:/data science/Credit_Card_Kaggle/UCI_Credit_Card.csv",header=TRUE)

#Checking the data types of all the variables of the dataset
myfun1<- function(x)
{
  class(x)
}
myfun1out<- t(lapply(credit_card,FUN=myfun1))
write.csv(myfun1out,file="D:/data science/Credit_Card_Kaggle/data_types.csv")

#View the dataframe
View(credit_card)

#Keep the whole dataframe in another dataframe for later use if needed
credit_card1<- credit_card 

#Remove the irrelevant variables
credit_card$ID<- NULL

#Convert the variables into relevant data types
credit_card$SEX<- as.factor(credit_card$SEX)
credit_card$EDUCATION<- as.factor(credit_card$EDUCATION)
credit_card$MARRIAGE<- as.factor(credit_card$MARRIAGE)
credit_card$AGE<- as.numeric(credit_card$AGE)
credit_card$PAY_0<- as.numeric(credit_card$PAY_0)
credit_card$PAY_2<- as.numeric(credit_card$PAY_2)
credit_card$PAY_3<- as.numeric(credit_card$PAY_3)
credit_card$PAY_4<- as.numeric(credit_card$PAY_4)
credit_card$PAY_5<- as.numeric(credit_card$PAY_5)
credit_card$PAY_6<- as.numeric(credit_card$PAY_6)
credit_card$default.payment.next.month<- as.factor(credit_card$default.payment.next.month)

#Rename the dependent variable
colnames(credit_card)[colnames(credit_card)=="default.payment.next.month"] <- "default"
table(credit_card$default)#no defaulters-77%, defaulters-22%

#Structure of the data
str(credit_card)

#Understanding the numerical variables data
myfun3<- function(x)
{
  var_type=class(x)   
  nmiss<- sum(is.na(x))
  MEAN<- mean(x,na.rm=T)
  SD<- sd(x,na.rm=T)
  pctl<- quantile(x,na.rm=T,p=c(0.01,0.05,0.5,0.09,0.95,0.98,0.99))
  MAX<- max(x,na.rm=T)
  MIN<- min(x,na.rm=T)
  return(c(var_type=var_type,nmiss=nmiss,MEAN=MEAN,SD=SD,MAX=MAX,MIN=MIN,pctl=pctl))
}
num_data<- select(credit_card,LIMIT_BAL,AGE,PAY_0,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,BILL_AMT6,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)
num_credit_card_data<- t(data.frame(lapply(num_data,FUN=myfun3)))
write.csv(num_credit_card_data,file="D:/data science/Credit_Card_Kaggle/descriptive.csv")  

#Checking for symmetric and non symmetric variables
hist(credit_card$AGE)

#All the variables seems to be non symmetric in nature

#Outlier Treatment for numerical variables with 99 percentile
outlier_treat2<- function(x)
{
  p99<-quantile(x,na.rm=T,p=c(0.99))
  p1<-quantile(x,na.rm=T,p=c(0.01))
  x[x>p99]<- p99
  x[x<p1]<-p1
  return (x)
}
num_data<-lapply(num_data,FUN=outlier_treat2)
num_credit_card_data<- t(data.frame(lapply(num_data,FUN=myfun3)))
write.csv(num_credit_card_data,file="D:/data science/Credit_Card_Kaggle/descriptive.csv")  

#Missing Value Detection
missing_treat1<- function(x)
{
  NMISSpct<- (sum(is.na(x))*100)/30000
  return(c(NMISSpct=NMISSpct))
}
out<-lapply(credit_card,FUN=missing_treat1)
print(out)
colSums(credit_card=='')
#So, there is no missing values in the whole dataset

#Separate the categorical variables from the dataset
fac_data<- select(credit_card,SEX,EDUCATION,MARRIAGE,default)

#Check for the significance of the numerical variables
anova_func<- function(x)
{
  anova_out<- aov(x~default,data=credit_card)
  o<-summary(anova_out)
  return (o)
}
lapply(num_data,FUN=anova_func)

#Bind the data
credit_card<- cbind(num_data,fac_data)

#Remove the variables which are insignificant known from anova test
credit_card$BILL_AMT5<- NULL
credit_card$BILL_AMT6<- NULL

#Check for the significance of the categorical variables
chisq.test(credit_card$default,credit_card$SEX,simulate.p.value = TRUE)
chisq.test(credit_card$default,credit_card$EDUCATION,simulate.p.value = TRUE)
chisq.test(credit_card$default,credit_card$MARRIAGE,simulate.p.value = TRUE)

#All the categorical variables are significant

#Create dummy variables for the categorical variables
library(fastDummies)
credit_card2<-dummy_cols(credit_card,select_columns =c("EDUCATION","MARRIAGE"),remove_first_dummy = TRUE )
write.csv(credit_card2,file="D:/data science/Credit_Card_Kaggle/credit_card2.csv")


#Convert the data types of categorical variables from integer to factor (the dummies created)
credit_card2$EDUCATION_1<- as.factor(credit_card2$EDUCATION_1)
credit_card2$EDUCATION_3<- as.factor(credit_card2$EDUCATION_3)
credit_card2$EDUCATION_5<- as.factor(credit_card2$EDUCATION_5)
credit_card2$EDUCATION_4<- as.factor(credit_card2$EDUCATION_4)
credit_card2$EDUCATION_6<- as.factor(credit_card2$EDUCATION_6)
credit_card2$EDUCATION_0<- as.factor(credit_card2$EDUCATION_0)
credit_card2$MARRIAGE_2<- as.factor(credit_card2$MARRIAGE_2)
credit_card2$MARRIAGE_3<- as.factor(credit_card2$MARRIAGE_3)
credit_card2$MARRIAGE_0<- as.factor(credit_card2$MARRIAGE_0)

#Check the significance of all the newly created categorical variables
chisq.test(credit_card2$default,credit_card2$EDUCATION_1,simulate.p.value = TRUE)
chisq.test(credit_card2$default,credit_card2$EDUCATION_3,simulate.p.value = TRUE)
chisq.test(credit_card2$default,credit_card2$EDUCATION_5,simulate.p.value = TRUE)
chisq.test(credit_card2$default,credit_card2$EDUCATION_4,simulate.p.value = TRUE)
chisq.test(credit_card2$default,credit_card2$EDUCATION_6,simulate.p.value = TRUE)
chisq.test(credit_card2$default,credit_card2$EDUCATION_0,simulate.p.value = TRUE)
chisq.test(credit_card2$default,credit_card2$MARRIAGE_2,simulate.p.value = TRUE)
chisq.test(credit_card2$default,credit_card2$MARRIAGE_3,simulate.p.value = TRUE)
chisq.test(credit_card2$default,credit_card2$MARRIAGE_0,simulate.p.value = TRUE)

#Remove the insignificant variables after chi square test
credit_card2$EDUCATION_6<- NULL
credit_card2$EDUCATION_0<- NULL
credit_card2$MARRIAGE_3<- NULL
credit_card2$EDUCATION<- NULL
credit_card2$MARRIAGE<- NULL


#Multicollinearity check
varss <- c("LIMIT_BAL","AGE","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6","PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")
credit_card3<- credit_card2[varss]
correlation_matrix<- cor(credit_card3,use = "complete.obs")
print(correlation_matrix)
library(xtable)
print(xtable(correlation_matrix), type="html")

#Remove the highly correlated independent variables
credit_card2$BILL_AMT2<- NULL
credit_card2$BILL_AMT3<- NULL
credit_card2$BILL_AMT4<- NULL
credit_card2$PAY_2<- NULL
credit_card2$PAY_3<- NULL
credit_card2$PAY_4<- NULL
credit_card2$PAY_5<- NULL
credit_card2$PAY_6<- NULL

#Split the data into training and testing dataset
smp_size<-floor(0.70*nrow(credit_card2))
set.seed(123)
train_ind<- sample(seq_len(nrow(credit_card2)),size=smp_size)
train<- credit_card2[train_ind,]
test<-credit_card2[-train_ind,]
names(credit_card2)

#Stepwise Regression
library(MASS)
fit<- glm(default~EDUCATION_1+EDUCATION_3+EDUCATION_5+EDUCATION_4+MARRIAGE_2+MARRIAGE_0+LIMIT_BAL+AGE+BILL_AMT1+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+SEX+PAY_0,data=train,family=binomial(logit))
step<-stepAIC(fit,direction="both")
summary(fit)
fit1<- glm(default ~ EDUCATION_5 + EDUCATION_4 + MARRIAGE_2 + MARRIAGE_0 + 
             LIMIT_BAL + AGE + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
             PAY_AMT4 + PAY_AMT5 + PAY_AMT6 + SEX+PAY_0,data=train,family=binomial(logit))
summary(fit1)
ls(fit1)
fit1$model
coeff<-fit1$coef
write.csv(coeff, file="D:/data science/Credit_Card_Kaggle/coeff2.csv")

#Concordance Checking
source("D:/data science/BA Class 7,8/Regression - Class Exercises/Concordance.R")
Concordance(fit1)

#After building the model equation, we will find probabilities (p-value) for training dataset
prob= predict(fit1,train,type="response")

#add the probability column to your training dataset
train1<- cbind(train,prob)
View(train1)

#Next, after finding the p-value for training dataset, do the decile analysis for training dataset
train1$default <- as.numeric(as.character(train1$default))
decLocations<- quantile(train1$prob,probs=seq(0.1,0.9,by=0.1))
train1$decile<- findInterval(train1$prob,c(-Inf,decLocations,Inf))
summary(train1$decile)
train1$decile<-factor(train1$decile)
decile_grp<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), default_cnt=sum(default), 
                             non_default_cnt=total_cnt -default_cnt )
decile_summ_train<-arrange(decile_summ_train,desc(decile))
View(decile_summ_train)
names(train1)
write.csv(decile_summ_train,file="D:/data science/Credit_Card_Kaggle/fit_train_DA1.csv",row.names = F)

#Next, predict the values for testing dataset
prob=predict(fit1,test,type="response")
test1<- cbind(test,prob)
View(test1)

#Next, do the decile analysis for testing dataset
test1$default<- as.numeric(as.character(test1$default))
decLocations<- quantile(test1$prob,probs=seq(0.1,0.9,by=0.1))
test1$decile<- findInterval(test1$prob,c(-Inf,decLocations,Inf))
summary(test1$decile)
test1$decile<-factor(test1$decile)
decile_grp<-group_by(test1,decile)
decile_summ_test<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), default_cnt=sum(default), 
                            non_default_cnt=total_cnt -default_cnt )
decile_summ_test<-arrange(decile_summ_test,desc(decile))
View(decile_summ_test)
write.csv(decile_summ_test,file="D:/data science/Credit_Card_Kaggle/fit_test_DA1.csv",row.names = F)

#So, from decile analysis report of training and testing dataset, find the cut off and then use that cut off to find predicted values for training dataset
#Finding the predicted values
train1$predicted_default<- ifelse(train1$prob>0.33271,1,0)

#Building the confusion matrix for training dataset
table(train1$prob>0.33271,train1$default)

#ROC Curve for training dataset
require(ROCR)
pred_train_fit<- prediction(train1$prob,train1$default)
performance_fit<- performance(pred_train_fit,"tpr","fpr")
plot(performance_fit)
abline(0, 1)
performance(pred_train_fit, "auc")@y.values


#So, from decile analysis report of training and testing dataset, find the cut off and then use that cut off to find predicted values for testing dataset
#Finding the predicted values
test1$predicted_default<- ifelse(test1$prob>0.31474,1,0)

#ROC Curve for testing dataset
require(ROCR)
pred_test_fit<- prediction(test1$prob,test1$default)
performance_fit<- performance(pred_test_fit,"tpr","fpr")
plot(performance_fit)
abline(0, 1)
performance(pred_train_fit, "auc")@y.values

#Building the confusion matrix for testing dataset
table(test1$prob>0.31474,test1$default)
#=============================#==========================#==================================#
#Report which we got from logitic regression model building
#For training dataset------
#Concordance is 73%
#Cut off value for training dataset is 0.33271
#AUC is 72%
#Sensitivity-> 54%
#Specificity-> 85%
#Accuracy is 79%
#For testing dataset------
#Cut off value for testing dataset is 0.31474
#AUC is 72%
#Sensitivity-> 51%
#Specificity-> 85%
#Accuracy is 79%
#=================#===================#=================#
