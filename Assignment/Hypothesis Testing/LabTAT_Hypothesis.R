#############################Hypothesis Testing#####################################

#Problem Statement:A hospital wants to determine whether there is any difference
#in the average Turn Around Time (TAT) of reports of the laboratories on their 
#preferred list. They collected a random sample and recorded TAT for reports 
#of 4 laboratories. TAT is defined as sample collected to report dispatch.
#Analyze the data and determine whether there is any difference in average TAT 
#among the different laboratories at 5% significance level.

# Data : LabTAT.csv
#####################################################################################


LabTATData <- read.csv(file.choose()) #LabTAT.csv
View(LabTATData)
colnames(LabTATData)<-c("Lab1","Lab2","Lab3","Lab4")#Change column names
View(LabTATData)
attach(LabTATData)
#The attach function allows to access variables of a data.frame without calling the data.frame
#LabTATData$Laboratory.1, Lab1

#X: lab reports i.e discrete
#Y: Difference in avg TAT 0f 4 labs Continous

################# Normality Test ###############################################
#Ho : Data is normally distributed
#Ha : Data is not normally distributed.
shapiro.test(Lab1)
#p-value = 0.55 > 0.05 So p high null fly =>Accept Ho 

shapiro.test(Lab2)
#p-value = 0.86 > 0.05 So p high null fly => Accept Ho 

shapiro.test(Lab3)
#p-value = 0.42 > 0.05 So p high null fly => Accept Ho 

shapiro.test(Lab4)
#p-value =  0.66 > 0.05 So p high null fly => Accept Ho i.e Data is normally distributed


################### Variance Test ##########################################
#Ho: Variance for Lab1,Lab2,Lab3,Lab4 are equal.
#Ha: Variance for Lab1,Lab2,Lab3,Lab4 are not qual.

#bartlett.test for checking variance of more than 2 population.
Stacked_Data <- stack(LabTATData)
View(Stacked_Data)
attach(Stacked_Data)

?bartlett.test
?aov

bartlett.test(values~ind,data=Stacked_Data) #test for homogeneity of variances
#p-value = 0.106 > 0.05 p high null fly => accept null hypothesis.
#Hence Variances of labs are equal.

#As there are more than 2 Discrete variables and output variable y is continous
#We carry One way ANOVA Test

#Ho: Mean TAT for all labs are same.
#Ha: Mean TAT for all labs is not same.
View(Stacked_Data)
anova_result <- aov(values~ind,data = Stacked_Data)
summary(anova_result)
# p-value= 2e-16  = 0 < 0.05 Reject Null hypothesis Mean TAT for all labs are
# not same.

# Hence there is difference in average TAT among different laboratories.