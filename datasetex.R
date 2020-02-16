data("airquality")
airquality <- datasets::airquality
head(airquality,n=10)
tail(airquality)#n number 
airquality[,c(1,2)]
airquality$Month #dataset.columnname

summary(airquality$Temp) 
summary(airquality)
