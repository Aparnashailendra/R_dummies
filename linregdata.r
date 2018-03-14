linregdata <- read.csv("C:/Users/dell/Desktop/Techdata_assignment/Regression/linregdata.csv")
names(linregdata)
head(linregdata)
tail(linregdata)
head(linregdata,2)
tail(linregdata)
summary(linregdata)

Fitlinreg <-lm(capped.loses~Average_age+Number.of.Vehicles+Gender_dummy+Married.dummy+Average_vehicle_Age+FuelType_Dummy, linregdata)#lm(dependent_variable~allindependent variable,filename)
summary(Fitlinreg) 
Fitlinreg <-lm(capped.loses~Average_age+Gender_dummy+Married.dummy+Average_vehicle_Age+FuelType_Dummy, linregdata)
summary(Fitlinreg)

install.packages("car")
vif(Fitlinreg)
summary(Fitlinreg)
linregdataestimated_losses<-675.88783 + -5.55966*linregdata$Average_age-50.88289*linregdata$Gender_dummy-78.40169 *linregdata$Married.dummy-15.14203*linregdata$Average_vehicle_Age+267.93514*linregdata$FuelType_Dummy 
