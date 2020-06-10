#########################Simple Linear Regression#####################
#Problem Statement:Build a prediction model for Churn_out_rate 

#Data : emp_data.csv
######################################################################
empData <- read.csv(file.choose()) #emp_data.csv
View(empData)
attach(empData)
summary(empData)

#Salary_hike   Churn_out_rate 
#Min.   :1580   Min.   :60.00  
#1st Qu.:1618   1st Qu.:65.75  
#Median :1675   Median :71.00  
#Mean   :1689   Mean   :72.90  
#3rd Qu.:1724   3rd Qu.:78.75  
#Max.   :1870   Max.   :92.00  


#install.packages("lattice")
require("lattice")
#Graphical exploration

boxplot(Salary_hike)
boxplot(Churn_out_rate, horizontal = T)

histogram(Salary_hike)
histogram(Churn_out_rate)

qqnorm(Salary_hike)
qqline(Salary_hike)
shapiro.test(Salary_hike)
# p-value = 0.5018 > 0.05 So p high null fly => accept null hypothesis.
#i.e. Salary hike data is normally distributed.

qqnorm(Churn_out_rate)
qqline(Churn_out_rate)
shapiro.test(Churn_out_rate)
#p-value = 0.73 > 0.05 So p high null fly => Accept Null hypothesis.
#i.e. Churn around rate data is normally distributed.

#Scatter plot
plot(Salary_hike,Churn_out_rate)
cor(Churn_out_rate,Salary_hike)
#-0.91 i.e DeliveryTime and Sort time are negatively correlated.
# if r > 0.85 then correlation is strong.r=-0.91 is strong negative correlation.

#Creating a simple linear model
empModel <- lm(Churn_out_rate ~ Salary_hike,data = empData)
summary(empModel)

#Call:
#  lm(formula = Churn_out_rate ~ Salary_hike, data = empData)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-3.804 -3.059 -1.819  2.430  8.072 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 244.36491   27.35194   8.934 1.96e-05 ***
#  Salary_hike  -0.10154    0.01618  -6.277 0.000239 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 4.469 on 8 degrees of freedom
#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 
#F-statistic:  39.4 on 1 and 8 DF,  p-value: 0.0002386

#Hence p-value < 0.05, X variable is significant and multiple R-squared value= 0.831.
#i.e this model predicts the output 83% correct.

confint(empModel,level=0.95)
#               2.5 %      97.5 %
#(Intercept) 181.2912317 307.4385905
#Salary_hike  -0.1388454  -0.0642399

predict(empModel,interval="predict")
pred <- predict(empModel,interval="predict")
pred <- as.data.frame(pred)
View(pred)
cor(pred$fit, Churn_out_rate) #0.91

#i.e Model predicts the output 83% correct.

#Applying transformation to increase R square value.
reg_log<-lm(Churn_out_rate~log(Salary_hike))
summary(reg_log)
#Multiple R-squared:  0.8486,	Adjusted R-squared:  0.8297 
confint(reg_log,level=0.95)
#2.5 %    97.5 %
#  (Intercept)       930.8584 1832.0540
#log(Salary_hike) -236.7512 -115.4682
pred2 <- predict(reg_log,interval="predict")
pred2 <- as.data.frame(pred2)
View(pred2)
cor(pred2$fit, Churn_out_rate) #0.92


quadModel<-lm(Churn_out_rate~Salary_hike + I(Salary_hike*Salary_hike))
summary(quadModel)

#Call:
#  lm(formula = Churn_out_rate ~ Salary_hike + I(Salary_hike * Salary_hike))

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.5523 -1.3280  0.3497  0.9029  2.8296 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   1.647e+03  2.281e+02   7.222 0.000174 ***
#  Salary_hike                  -1.737e+00  2.657e-01  -6.538 0.000322 ***
#  I(Salary_hike * Salary_hike)  4.754e-04  7.720e-05   6.158 0.000464 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.886 on 7 degrees of freedom
#Multiple R-squared:  0.9737,	Adjusted R-squared:  0.9662 
#F-statistic: 129.6 on 2 and 7 DF,  p-value: 2.949e-06

confint(quadModel,level=0.95)
#                                2.5 %        97.5 %
#  (Intercept)                   1.107738e+03  2.186285e+03
#Salary_hike                  -2.365306e+00 -1.108872e+00
#I(Salary_hike * Salary_hike)  2.928508e-04  6.579259e-04

pred3<-predict(quadModel,interval="predict")
pred3 <- as.data.frame(pred3)
pred3
#fit      lwr      upr
#1  89.17035 83.78480 94.55590
#2  84.66327 79.65803 89.66851
#3  82.55234 77.66358 87.44111
#4  76.79003 72.03031 81.54976
#5  73.42388 68.63567 78.21210
#6  69.08774 64.20255 73.97292
#7  67.12501 62.19286 72.05716
#8  64.63730 59.66195 69.61265
#9  60.50952 55.48124 65.53780
#10 61.04055 54.97443 67.10667

cor(pred3$fit,Churn_out_rate) #i.e.0.98

#Quad model has increased Multiple R-squared from 0.8312 to 0.98. So quad model is good model will predict 
# the output 98% correct.









