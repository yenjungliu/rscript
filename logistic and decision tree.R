#To do logistic regression and decision tree, I will use the spam data which shows you how to idenfity spam mail based on some characristic.
pa<-c("ggplot2","dplyr","car","gvlma")
lapply(pa,require,character.only=TRUE)
data_spam<- read.csv("/Users/yenjungliu/Downloads/spam7.csv")
str(data_spam)
#As you can see ,there are 8 variable in the data set but I will delect the first variable "x", the ID variable. 
#crl.tot shows total length of words in capitals
#dollar shows the occurrences of the \$ symbol
#bang shows the occurrences of the ! symbol
#money shows the occurrences of the word "money"
#n000 shows the occurrences of the string 000
#make shows the occurrences of the word "make"
#and the last colum yesno shows the outcome variable, n means not spam and y means it's spam mail
df<-data_spam[-1]
str(df)
#To start with logistic regression, I will devide the data into two groups. 70% of the data will be classified as training group, which is used to create classification schemes and the rest 30% will be placed in validation group which is use to evaluate the effectiveness of schemes.  
train<-sample(nrow(df),0.7*nrow(df))
df.train<-df[train,]
df.vali<-df[-train,]
table(df.train$yesno)
table(df.vali$yesno)
#As you can see, there are 3220 cases in train sample which consist of 1274 spam mail and 1946 not spam mail. 
#Furthermore, there are 1381 cases in validation sample which contains 539 spam mail and 842 not spam mail
#I will use the train sample to create the logistic regression and also the decision tree and the validation sample will be use to evaluate the effectiveness of my work. 
logit<-glm(yesno~.,data=df.train,family=binomial())
summary(logit)
# As you can see, all the variables are significant different from zero except the make variable which has p value greater than 0.05.
#Let's try not to include the "make" variable and see if our AIC value will increase. 
df.make<-df.train[-6]
logit_nomake<-glm(yesno~.,data=df.make,family=binomial())
summary(logit_nomake)
#Now, the AIC has decreased from 2724.3 to 2722.6 which is preferred. 
#Next, I want to evaluate the effectiveness of the logistic regression.  