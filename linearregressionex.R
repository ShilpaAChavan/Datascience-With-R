
####LINEAR REGRESSION ####
# model <-lm(y~x,data=dataset name)
model<- lm(sunday~daily,data=NewspaperData)
summary(model)


sun=13.84+1.34*200 #sun=intercept(B0)+slope(B1)*daily
sun
