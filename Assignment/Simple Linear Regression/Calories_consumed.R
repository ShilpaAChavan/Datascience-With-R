#################################Simple Linear Regression#####################################

#Problem Statement:predict weight gained using calories consumed

#Data : calories_consumed.csv
#####################################################################################

caloriesData <- read.csv(file.choose()) #calories_consumed.csv
View(caloriesData)
colnames(caloriesData) <- c("WeightGained","CaloriesConsumed")

attach(caloriesData)
View(caloriesData)
summary(caloriesData)

#WeightGained    CaloriesConsumed
#Min.   :  62.0   Min.   :1400    
#1st Qu.: 114.5   1st Qu.:1728    
#Median : 200.0   Median :2250    
#Mean   : 357.7   Mean   :2341    
#3rd Qu.: 537.5   3rd Qu.:2775    
#Max.   :1100.0   Max.   :3900   

install.packages("lattice")
require("lattice")
#Graphical exploration
boxplot(WeightGained )
#Most of observation have weight gained data spread in between 200 to 600.Median is 200.Data dont have 
#outlier.

boxplot(CaloriesConsumed, horizontal = T)
#Most of observations of calories consumed is between 1700 and 2800 approx.Median is 2250.Data dont have
#outlier.

histogram(WeightGained)
#Weighed gained data is positively skewed.

histogram(CaloriesConsumed)
#Calories consumed data is positively skewed.

qqnorm(WeightGained)
qqline(WeightGained)
shapiro.test(WeightGained)
# p-value = 0.006 < 0.05 So p low null go => Reject null hypothesis.
#i.e. Weight gain data is not normally distributed.

qqnorm(CaloriesConsumed)
qqline(CaloriesConsumed)
shapiro.test(CaloriesConsumed)
#p-value = 0.48 > 0.05 So p high null fly => Accept Null hypothesis.
#i.e. Calories consumed data is normally distributed.

#Scatter plot
plot(CaloriesConsumed,WeightGained)
cor(WeightGained,CaloriesConsumed)
#0.94699 i.e Weight gained and calories consumed are positively correlated.
# r is greater than 0.85 then Corelation is strong. 

#Model Building 
CalConsumedModel <- lm(WeightGained ~ CaloriesConsumed, data= caloriesData)
summary(CalConsumedModel)
#R-squared:  0.896 i.e. Coefficient of determination.
#*** tell that both intercept and CaloriesConsumed are significant and important  to consider in model.

#Predict weight gained
confint(CalConsumedModel,level=0.95)
pred <- predict(CalConsumedModel,interval="predict")
pred <- as.data.frame(pred)
View(pred)
cor(pred$fit,WeightGained)
# 0.946991

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(WeightGained ~ sqrt(CaloriesConsumed), data=caloriesData)
summary(reg_sqrt)
#R-squared:  0.8567
confint(reg_sqrt,level=0.95)
pred1 <- predict(reg_sqrt,interval="predict")
pred1 <- as.data.frame(pred1)
View(pred1)
cor(pred1$fit,WeightGained)#0.92

reg<-lm(log(WeightGained)~CaloriesConsumed + I(CaloriesConsumed*CaloriesConsumed), data=caloriesData)
summary(reg) 
#  R-squared:  0.87
confint(reg,level=0.95)
pred2<-predict(reg,interval="predict")
pred2 <- as.data.frame(pred2)
pred2 <- exp(pred2$fit)
pred2


quadModel<-lm(WeightGained~CaloriesConsumed + I(CaloriesConsumed*CaloriesConsumed))
summary(quadModel)
#Call:
#  lm(formula = WeightGained ~ CaloriesConsumed + I(CaloriesConsumed * CaloriesConsumed))

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-90.321 -63.843  -3.609  52.120 120.222 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)                             2.033e+02  2.436e+02   0.834  0.42185   
#CaloriesConsumed                       -2.919e-01  2.021e-01  -1.444  0.17653   
#I(CaloriesConsumed * CaloriesConsumed)  1.395e-04  3.918e-05   3.561  0.00447 **
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 79.43 on 11 degrees of freedom
#Multiple R-squared:  0.9521,	Adjusted R-squared:  0.9433 
#F-statistic: 109.2 on 2 and 11 DF,  p-value: 5.546e-08
  
confint(quadModel,level=0.95)
#                                            2.5 %       97.5 %
#  (Intercept)                          -3.329553e+02 7.394722e+02
#CaloriesConsumed                       -7.367129e-01 1.529250e-01
#I(CaloriesConsumed * CaloriesConsumed)  5.328126e-05 2.257552e-04
  
pred3<-predict(quadModel,interval="predict")
pred3 <- as.data.frame(pred3)
pred3

#fit        lwr       upr
#1    79.33345 -115.15333  273.8202
#2   269.95364   83.77882  456.1285
#3   823.64945  629.80153 1017.4974
#4   236.35984   51.13324  421.5864
#5   345.51232  157.77722  533.2474
#6    93.39470  -95.99261  282.7820
#7    68.06257 -133.64283  269.7680
#8   152.32063  -31.28353  335.9248
#9   479.77809  291.24523  668.3109
#10 1186.94394  953.63989 1420.2480
#11  104.89784  -82.01852  291.8142
#12  152.32063  -31.28353  335.9248
#13  432.23247  243.77631  620.6886
#14  583.24042  394.65118  771.8297

cor(pred3$fit,WeightGained) #0.97

#Hence after doing transformation multiple R square value has increased from 0.89 to 0.95.i.e. The quadmodel 
# can predict output 95% correct.

  









