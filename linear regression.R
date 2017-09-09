pack<-c(ggplt2,dplyr,car,gvlma)
library(ggplot2)
library(dplyr)
library(car)
install.packages("gvlma")
library(gvlma)
# In here, I am going to use a linear regression to predict the crime rate in U. S. metropolitan area.
data_crime<- read.csv("/Users/yenjungliu/Downloads/Freedman.csv")
h <- function(x){
  glimpse(x)
}
h(data_crime)
#The first colume shows the location of the observed area. 
#The second colume shows the population in each area
#The third colume shows the percent of nonwhite population
#The fourth colume shows the density of the area (population per square)
#The last colume shows the outcome, which is the crime rate per 100,000 people
#Moreover, as you can see from the thrid row, the value for population and density are not available and the not available value will affect my accuracy of prediction. I will use the following code to solve this issue. 
#We can choose to remove the data which contains NA
data_na<-na.omit(data_crime)
head(data_na)
table(is.na(data)) 
#As you can see, the third row is omit due to the and we can see there is no NA value in this data frame. Great! Let's get back to the data set and get started. 
model<-lm(crime~population+(nonwhite)+density, data=data_na)
summary(model)
#Let's look at the p value for each variable, the p value for density doesn't show significant differerence and it means that density is not a good variable to predict the crime rate or you can said it's not correlated to the crime rate. 
#Furthermore, the adjusted R-squared is equal to 0.2047 which means the model only can predict 20.47% of the observed data. It does't look good, so how about let's remove the density variable and see how it works. 
model_desity<-lm(crime~population+nonwhite, data=data_na)
summary(model_desity)
#As you can see, when I remove density, the adjusted R-squared increased from 0.2047 to 0.212, however, it's still low. To improve adjusted R-squared, I will use the following steps. 
boxTidwell(crime~population+nonwhite,data=data_na)
data_na %<>% mutate( pop = population^(-0.5) ,nonw= nonwhite^(-0.36)) 
li<-data_na %$% lm(crime~pop+nonw)
summary(li)
h(data_na)
#As you see, I transform the linear modeal to crime=population^(-0.5)+nonwhite^(-0.36) and the Adjusted R-squared to 0.3977, which is fair good enough for a linear model with 2 variables.
#Now, we have to check the 3 asssumption for the validation of linear model. 
gvmodel <- gvlma(li)%>%summary()
# As you can see, all the assumptions are acceptable. 
# Furthermore, besides the assumptions, we need to check multicollinearity to make sure that one variable is not correlated to another variable. 
# I will use the square root of VIF and the general rule, when the value is greater than 2, it indicates the multicollinearity problem.  
sqrt(vif(li)) > 2
#As you can see, it does not show the multicollinearity problem. 
#Inconclusion, the linear model will be crime rate=4823.5-31916.5 population^(-0.5)-1570.7 nonwhite^(-0.36). 


