#################################Hypothesis Testing#####################################

#Problem Statement:TeleCall uses 4 centers around the globe to process customer order
#forms. They audit a certain %  of the customer order forms. Any error in order form 
#renders it defective and has to be reworked before processing.  The manager wants to 
#check whether the defective %  varies by centre. Please analyze the data at 5% 
#  significance level and help the manager draw appropriate inferences

# Data : Costomer+OrderForm.csv
#####################################################################################
library(readxl)
customerData <- read.csv(file.choose()) #Costomer+OrderForm.csv
View(customerData)


customOrder <- as.data.frame(lapply(customerData, as.numeric))
View(customOrder)

#Ho : Defective percentage are same across center
#Ha : Defective percentage are varies across center

?chisq.test
chisq.test(customOrder)
##
#data:  customOrder
#X-squared = 44.655, df = 897, p-value = 1

#Warning message:
#In chisq.test(customOrder) : Chi-squared approximation may be incorrect


chisq.test(customOrder,simulate.p.value = TRUE)
#data:  customOrder
#X-squared = 44.655, df = NA, p-value = 1
#p-value = 1 >0.05 so p high null fly => accept NULL hypothesis


#Hence the defective percentage are same across the centers.