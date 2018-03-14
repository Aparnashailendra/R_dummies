# set working directory
setwd("C:/Users/dell/Desktop/Techdata_assignment/todaytarget/R_code")
#how to check working directory
getwd()
# Read the data file
Default_data<-read.csv("data.csv")
# Chcek what is there in the Default file data
str(Default_data)
# To check data is polulated or imported properly
View(Default_data)#To check data
head(Default_data)#To check data
tail(Default_data)#To check data
#Check the summary stastistics of file
summary(Default_data)#Find out mean,median,misiing values, find out frequency distribution if it is categorical
#Find variable names
names(Default_data)
#Generate a plot of dependent variable
plot(Default_data$Losses)
# check the quantile to find out outlier limit
quantile(Default_data$Losses,c(0,0.05,0.1,0.25,0.5,0.75,0.90))
#cREATING A CAPPED LOSSES COLUMN WITH 1200 cap
Default_data$CappedLosses <-ifelse(Default_data$Losses>1200,1200,Default_data$Losses)
#Check if cappedlossses column has been created successfully or not(max should be 1200)
summary(Default_data$CappedLosses)
names(Default_data)
#  Creating new object and deleting losses and s.no.
Default_data3<-Default_data[,-c(1,9)]
#univariate analysis:analysis of one variable at a time


#Starts of bivariate analysis:analysis two variables at a time
plot(Default_data3$Age,Default_data3$CappedLosses)
plot(Default_data3$Years.of.Driving.Experience,Default_data3$CappedLosses)
plot(Default_data3$Number.of.Vehicles,Default_data3$CappedLosses)
plot(Default_data3$Gender,Default_data3$CappedLosses)
plot(Default_data3$Married,Default_data3$CappedLosses)
plot(Default_data3$Vehicle.Age,Default_data3$CappedLosses)
plot(Default_data3$Fuel.Type,Default_data3$CappedLosses)
#check the headings of new object
names(Default_data3)
# install  package for transformation of data
install.packages("reshape")
library(reshape)
data.m<-melt(Default_data3, id =c(1:7), measure=c(8))
#Lets look different values of new project
head(data.m)
str(data.m)
#Lets cast our data
#lets aggregate data by cast losses by age
cast(data.m, Age~variable, fun.aggregate = sum)#taking sum
data.c<-cast(data.m, Age~variable,mean)#taking mean
data.c
#lets aggregate capped losses by age (sum and mean)
data.c<-cast(data.m, Age~variable,c(sum,mean))
data.c      
#Bucketing of age
# create age bands form 16 to 25,26 to 59 and 60+
Default_data3$Ageband <- ifelse(Default_data3$Age<=25,"16-25",ifelse(Default_data3$Age>=60,"60+","26-59"))
summary(Default_data3$Ageband)
#Lets see we are correctly convert or not
head(Default_data3)
tail(Default_data3)
#Create Age band, mean across ageband
data.ageband<-aggregate(Age~Ageband,data=Default_data3,mean)
data.ageband
#just merge two variable as vlookup in excel(the key variable is age variable on which two functions are joined)
Default_data3<-merge(Default_data3,data.ageband, by ="Ageband")
View(Default_data3)
#we can export data from r to excel
write.csv(Default_data3,"Datar1.csv")
#Simlarly we can convert vehicle age to vehicle age band
#Covert categorical variable into dummy variable
Default_data3$GenderDummy<-ifelse(Default_data3$Gender=="F",1,0)
Default_data3$MarriedDummy<-ifelse(Default_data3$Married=="Married",0,1)
View(Default_data3)
Default_data3<-Default_data[,-c(5,6)]
View(Default_data3)
#Check the headings and summary
summary(Default_data3)
names(Default_data3)
head(Default_data3)
# we will use variables which is converted into dummy variables
# read the final data
DefaultData4<-read.csv("Linear_Reg_Sample_Data.csv")
#Look at the column headings
head(DefaultData4)
# intall package car to check multicollinarity and to check the vif
install.packages("car")
library("car")
#create linear function for vif-to check multicollinarity
vif_data <- lm(Capped_Losses ~ Years_Drv_Exp+Number_Vehicles+Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)
#check vif if vif >2 means presence of multicollinarity
vif(vif_data)
#Average_Age and Years_Drv_Exp are the variablle which causing multicollinarity finding which one to keep
#comapre R square of average age and years of driving experience
#better perform single variable regression
age1<-lm(Capped_Losses~Average_Age,data=DefaultData4)
drv1<-lm(Capped_Losses~Years_Drv_Exp,data=DefaultData4)
summary(age1)
summary(drv1)
# keep avergae since it has high r square and drop Years_Drv_Exp
# I n same way we can decide to keep age band as compared to age and vehicle ageband
#Run linear model wout years of driving experienc
linr1 <- lm(Capped_Losses ~ Number_Vehicles+Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)
summary(linr1)
# RUn linear model wout Number_Vehicles(insginificant variable p>alphanumeric )
linr2 <- lm(Capped_Losses ~ Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)
summary(linr2)
# Residuals-storing it in a dataframe(ln.res)
ln.res<-resid(linr2)
#predicted values storing it in a dataframe(ln.pred)
ln.pred<-predict(linr2)

