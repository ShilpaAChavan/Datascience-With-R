#################################Hypothesis Testing#####################################

#Problem Statement:Sales of products in four different regions is tabulated for males and females.
#Find if male-female buyer rations are similar across regions.

# Data : BuyerRatio.csv
#####################################################################################


BuyerRatio <- read.csv(file.choose()) #BuyerRatio.csv
attach(BuyerRatio)
View(BuyerRatio)

region1 <- East
region2 <- West
region3 <- North
region4 <- South

region= data.frame(region1,region2,region3,region4)
View(region)

#Ho : Proportions of male and female are same.
#Ha : Proportions of male and female are not same.

?chisq.test
chisq.test(region)

# p-value = 0.66 > 0.05 p high null fly => accept null hypothesis

#Hence the male female buyer ratio is similar accross region.



##Second Approach:
stacked_data<- stack(BuyerRatio)
View(stacked_data)
attach(stacked_data)
chisq.test(table(values,ind))
#p-value = 0.2931 > 0.05 p high null fly =>accept null hypothesis

#Hence the male female buyer ratio is similar accross region.
#Warning message:
 # In chisq.test(table(values, ind)) :
 # Chi-squared approximation may be incorrect
                                                                                                                                                                                                 ``  `
#simulate.p.value = TRUE
#data:  table(values, ind)
#X-squared = 24, df = NA, p-value = 1















