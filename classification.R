#To do logistic regression and decision tree, I will use the spam data which shows you how to idenfity spam mail based on some characristic.
installed.packages("gvlma")
library("ggplot2","dplyr","car","gvlma")
data_spam<- read.csv("/Users/yenjungliu/Downloads/spam7.csv")
str(data_spam)
#As you can see ,there are 6 independents variable which may help us to predict the value and also the outcome of spam mail. 
#To start with logistic regression, I will devide the data into two groups. 70% of the data will be classified as training group, which is used to create classification schemes and the rest 30% will be placed in validation group which is use to evaluate the effectiveness of schemes.  
df<-data_spam[-1]
train<-sample(nrow(df),0.7*nrow(df))
df.train<-df[train]
df.vali<-df[-train]
#why nrow?