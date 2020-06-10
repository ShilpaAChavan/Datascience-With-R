#########################Simple Linear Regression#####################
#Problem Statement:Build a prediction model for Salary_hike
#Data : Salary_Data.csv
######################################################################
salData <- read.csv(file.choose()) #Salary_Data.csv
View(salData)
colnames(salData) <- c("YrsExp","Salary")
attach(salData)
summary(salData)

#YrsExp           Salary      
#Min.   : 1.100   Min.   : 37731  
#1st Qu.: 3.200   1st Qu.: 56721  
#Median : 4.700   Median : 65237  
#Mean   : 5.313   Mean   : 76003  
#3rd Qu.: 7.700   3rd Qu.:100545  
#Max.   :10.500   Max.   :122391 

#install.packages("lattice")
require("lattice")
#Graphical exploration

boxplot(YrsExp)
boxplot(Salary, horizontal = T)

histogram(YrsExp)
histogram(Salary)

#Scatter plot
plot(YrsExp,Salary)
cor(YrsExp,Salary)
# if r > 0.85 then correlation is strong.r=0.97 is strong positive correlation.

#Creating a simple linear model
salModel <- lm(Salary ~ YrsExp,data = salData)
summary(salModel)


#Call:
#  lm(formula = Salary ~ YrsExp, data = salData)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.12974 -0.46457  0.04105  0.54311  0.79669 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.383e+00  3.273e-01  -7.281  6.3e-08 ***
#  Salary       1.013e-04  4.059e-06  24.950  < 2e-16 ***
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.5992 on 28 degrees of freedom
#Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554 
#F-statistic: 622.5 on 1 and 28 DF,  p-value: < 2.2e-16


confint(salModel,level=0.95)
#                 2.5 %   97.5 %
#(Intercept) 21136.061    30448.34
#YrsExp       8674.119    10225.81

predict(salModel,interval="predict")
pred <- predict(salModel,interval="predict")
pred <- as.data.frame(pred)
View(pred)

#     fit       lwr       upr
#1   36187.16  23698.92  48675.40
#2   38077.15  25628.63  50525.67
#3   39967.14  27556.52  52377.76
#4   44692.12  32368.22  57016.03
#5   46582.12  34289.64  58874.59
#6   53197.09  40999.70  65394.48
#7   54142.09  41956.37  66327.80
#8   56032.08  43868.25  68195.91
#9   56032.08  43868.25  68195.91
#10  60757.06  48639.42  72874.70
#11  62647.05  50544.46  74749.65
#12  63592.05  51496.24  75687.86
#13  63592.05  51496.24  75687.86
#14  64537.05  52447.52  76626.57
#15  68317.03  56247.70  80386.36
#16  72097.02  60039.93  84154.10
#17  73987.01  61933.05  86040.96
#18  75877.00  63824.18  87929.82
#19  81546.98  69485.57  93608.39
#20  82491.97  70427.39  94556.56
#21  90051.94  77944.06 102159.83
#22  92886.93  80754.66 105019.20
#23 100446.90  88228.15 112665.65
#24 103281.89  91022.76 115541.02
#25 108006.87  95670.98 120342.77
#26 110841.86  98454.30 123229.42
#27 115566.84 103084.00 128049.68
#28 116511.84 104008.59 129015.09
#29 123126.81 110468.27 135785.35
#30 125016.80 112309.98 137723.63

cor(pred$fit, Salary) #0.97


#Hence Multiple R squared value for the built model is 0.957. i.e. The model can predict output 
#97% correct




