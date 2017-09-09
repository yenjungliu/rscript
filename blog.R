library(dplyr)
library(magrittr)
data(airquality)
summary(airquality)
head(airquality)

#It shows the first 5 data of the data set. 
#We can see the mean ozone level, solar radiation, average wind speed and maximum temperature everyday since May 1st. 
#To see the distribution of each variable, I use the box plot. In this example, I want to find out the distribution of wind speed each month. 
airquality %$% boxplot(formula = Wind ~ Month, #Y value~X value
                       xlab = "Month",          
                       ylab = "Windspeed (mph)") 
# The interest point is the winds peed on June. From the box plot, the data is concentrated in the center of the distribution; however, it has two outliers, which are the maximum and minimum point of this data. It shows that usually the wind speed on June is around 10 mph, but there are two days, which have extreme wind speed. 
#Furthermore, we want to know if the wind speed is correlated with other variable, to find out, I can use the scatter pot. 
airquality %$% plot(x=.$Ozone,            
                    y=.$Wind,             
                    main="Ozone to wind",   
                    xlab="Ozone(ppb)",            
                    ylab="Windspeed (mph)")      
#In this example, I want to find out the correlation between ozone and wind. From the scatter plot, we see that with higher ozone level, the wind speed is weaker. I can also put an abline here to see if the points fit the line or not. 
lm.model  <-
  airquality %$% lm(Wind~Ozone)  
abline(lm.model)   
#If more point fit the abline, it means the ozone and wind is more related to each other. 
#If we want to focus a particular month, for example, June, we can use the following steps. 
sep_data <- airquality %>% filter(Month == 9)
points(x=sep_data$Ozone,                       
       y=sep_data$Wind, 
       pch=2,                  
       col="red")  
#If I want to show the reader that the red point represent the data of September, I can put the legend on the graph. 
legend("topright",                                # the position of the legend
       pch = c(1,2),                              # the shape of the point
       col = c("red","black"),                    # color of the point 
       legend = c( "September", "Other Month")    # name of the color
)

# So what if I want to have 2 graph at the same time, say the scatter plot for ozone to wind and ozone to temperature?
par(mfrow = c(2,1))                               # First you set that it should be 2 row and 1 colum        
plot(airquality$Ozone, airquality$Wind, main = "Ozone to Wind", xlab = "Ozone", ylab = "Wind") 
plot(airquality$Ozone, airquality$Temp, main = "Ozone to Temperature", xlab = "Ozone", ylab = "Temperature")

#Now, I want to talk about one of the most popular package in R, ggplot2. We will still use the airquality example, but this time, I will work with ggplot2. To start it, I need a canvas first and I am doing to draw a histogram here. 
library(ggplot2)
library(dplyr)

airquality %<>% mutate( Month = factor(.$Month))
canvas <- ggplot(data=airquality)
canvas+geom_histogram(aes(Ozone,fill=Month,binwidth = 25))

# It can also show the histgram for each month
canvas+geom_histogram(aes(Ozone,fill=Month,binwidth = 25))+facet_grid(.~Month)

#The density plot which shows the distribution of Ozone level in each month. We can see that the Ozone level on May, June and Sepemter is more likely to be low.  
canvas+geom_density(aes(Ozone,color=Month))

# The box plot can also be applied here to see the distribution of Ozone level each month.
canvas+ geom_boxplot(aes(Month,Ozone,color=Month))

# I can also do the scatter plot and a regression line. We can see that not many points fit the line so it probabaly isn't a good regression line. 
ggplot(data=airquality)+ geom_point(aes(x=Ozone,y=Wind,color=Month))+geom_smooth(aes(x=Ozone,y=Wind),method = lm)

# I can also put a line to connect all the point. As you can see, it's fluactual and doesn't show any trend. 
ggplot(data=airquality,aes(x=Ozone,y=Wind,color=Month))+ geom_point()+geom_line()

#Lastely, I am going to show you how to do bar plot. First we have to calculate the sum of the Ozone, then we can see the Ozone sum each month.    
DF <- airquality %>% group_by(Month) %>% summarise(Ozone = sum(Ozone, na.rm = TRUE) )     
ggplot(data=DF) +
  geom_bar(aes(x=factor(1),
               y=Ozone,
               fill=factor(Month)),
           stat = "identity"
  ) +
  coord_polar("y", start=0)    
