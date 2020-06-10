#################################Simple Linear Regression#####################################

#Problem Statement:Predict delivery time using sorting time

#Data : delivery_time.csv
#####################################################################################

deliveryTimeData <- read.csv(file.choose()) #delivery_time.csv
View(deliveryTimeData)
colnames(deliveryTimeData) <- c("DeliveryTime","SortTime")
attach(deliveryTimeData)
View(deliveryTimeData)
summary(deliveryTimeData)

#DeliveryTime      SortTime    
#Min.   : 8.00   Min.   : 2.00  
#1st Qu.:13.50   1st Qu.: 4.00  
#Median :17.83   Median : 6.00  
#Mean   :16.79   Mean   : 6.19  
#3rd Qu.:19.75   3rd Qu.: 8.00  
#Max.   :29.00   Max.   :10.00


install.packages("lattice")
require("lattice")
#Graphical exploration

boxplot(DeliveryTime )
boxplot(SortTime, horizontal = T)

histogram(DeliveryTime)
histogram(SortTime)

qqnorm(SortTime)
qqline(SortTime)
shapiro.test(SortTime)
# p-value = 0.18 < 0.05 So p low null go => Reject null hypothesis.
#i.e. SortTime data is not normally distributed.

qqnorm(DeliveryTime)
qqline(DeliveryTime)
shapiro.test(DeliveryTime)
#p-value = 0.8963 > 0.05 So p high null fly => Accept Null hypothesis.
#i.e. Delivery time data is normally distributed.

#Scatter plot
plot(SortTime,DeliveryTime)
cor(SortTime,DeliveryTime)
#0.82 i.e DeliveryTime and Sort time are positively correlated.
#If r > 0.85 then correlation is strong.r=0.82 is moderate correlation.

#Creating a simple linear model
deliveryTimeModel <- lm(DeliveryTime ~ SortTime,data = deliveryTimeData)
summary(deliveryTimeModel)

#Call:
#  lm(formula = DeliveryTime ~ SortTime, data = deliveryTimeData)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-5.1729 -2.0298 -0.0298  0.8741  6.6722 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.5827     1.7217   3.823  0.00115 ** 
#  SortTime      1.6490     0.2582   6.387 3.98e-06 ***
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2.935 on 19 degrees of freedom
#Multiple R-squared:  0.6823,	Adjusted R-squared:  0.6655 
#F-statistic:  40.8 on 1 and 19 DF,  p-value: 3.983e-06
  
#Hence p-value < 0.05, X variable is significant and multiple R-squared value= 0.68%.
#i.e this model predicts the output 68% correct.
  
  
# Increasing R2 value
# Deletion Diagnostics for identifying influential variable
install.packages("car")
library(car)
influence.measures(deliveryTimeModel)
influenceIndexPlot(deliveryTimeModel) # Index Plots of the influence measures
influencePlot(deliveryTimeModel)# A user friendly representation of the above
?influencePlot

#By checking the plots, we are going to remove 5,19 & 21 observation and build the model again.
deliverySortTime <- lm(DeliveryTime ~ SortTime,data= deliveryTimeData[c(-5,-9,-21),])
summary(deliverySortTime)

#Call:
#  lm(formula = DeliveryTime ~ SortTime, data = deliveryTimeData[c(-5,-9, -21), ])

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.3407 -1.5027  0.2275  0.9328  3.6815 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.0240     1.1751   5.126 0.000102 ***
#  SortTime      1.6741     0.1872   8.941 1.27e-07 ***
#  ---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.839 on 16 degrees of freedom
#Multiple R-squared:  0.8332,	Adjusted R-squared:  0.8228 
#F-statistic: 79.94 on 1 and 16 DF,  p-value: 1.273e-07

#Hence after removing 5,19 & 21 observation, Multiple R-square value increased to 0.8332.This model will
#predict 83% output time as correct.
