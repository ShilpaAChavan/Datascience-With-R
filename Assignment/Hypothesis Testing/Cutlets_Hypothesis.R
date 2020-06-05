#################################Hypothesis Testing#####################################

#Problem Statement:A F&B manager wants to determine whether there is any significant 
#difference in the diameter of the cutlet between two units. A randomly selected 
#sample of cutlets was collected from both units and measured? Analyze the data 
#and draw inferences at 5% significance level. Please state the assumptions and 
#tests that you carried out to check validity of the assumptions.
# Data : Cutlets.csv
#####################################################################################

CutletData <- read.csv(file.choose()) #Cutlets.csv
View(CutletData)
attach(CutletData)
#The attach function allows to access variables of a data.frame without calling the data.frame
#CutletData$Unit.A, Unit.A

#x: cutlet - Discrete datatype
#Y: Unit.A, Unit.B diameter - Continous datatype 
# Compare the two population Unit.A and Unit.B

################# Normality Test ###############################################
#Ho : Data is normally distributed
#Ha : Data is not normally distributed.
shapiro.test(Unit.A)
#p-value =0.32 > 0.05 So p high null fly => It follows Normal Distribution.

shapiro.test(Unit.B)
#p-value = 0.52 > 0.05 So p high null fly => It follows Normal Distribution.


################### Variance Test ##########################################
#Ho : var(Unit.A) = var(Unit.B)
#Ha : var(Unit.A) != var(Unit.B)

var.test(Unit.A,Unit.B)
#p-value = 0.31 > 0.05 So p high null fly accept null hypthesis
# Hence variance of Unit.A equal to variance Unit.B


################# 2- Sample T Test #########################################
#Ho : mean(Unit.A) = mean(Unit.B) i.e equal means
#Ha : mean(Unit.A) != mean(Unit.B) i.e not equal means
# Signifiance level = 5% i.e alpha =0.05 
?t.test
t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct = True)
# alternative = "two.sided" means we are checking for equal and unequal means
#p-value = 0.472 > 0.05 So p high null fly => equal means

#Accept Null hypothesis There is no significant difference in the diameter
# of cutlets of Unit A and Unit B i.e the diameters are same.
