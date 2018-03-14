setwd("C://Users//dell//Desktop//Techdata_assignment//Regression//CART")
cust_data<-read.csv("Default_On_Payment_CHAID.csv")
View(cust_data)
cust_data$Default_On_Payment<-ifelse(cust_data$Default_On_Payment==1,"Default","Non Default")
View(cust_data)
install.packages("foreign")
install.packages(Hmisc)
install.packages(CHAID)
install.packages(Rcmdr)
ctrl<-chaid_control(minbucket=100,minsplits=100,alpha2=0.5,alpha4=.05)
cust_data$Default_On_Payment<-ordered(cust_data$Default_On_Payment)
cust_chaid<-chaid(Default_On_Payment~Status_checking_Acc+Credit_History,data=cust_data,control=ctrl)
plot(cust_chaid)
cust_chaid<-chaid(Default_On_Payment~Status_checking_Acc+Credit_History+Job,data=cust_data,control=ctrl)
plot(cust_chaid)
cust_chaid<-chaid(Default_On_Payment~Credit_History+Job,data=cust_data,control=ctrl)
plot(cust_chaid)
cust_chaid<-chaid(Default_On_Payment~Credit_History,data=cust_data,control=ctrl)## try one by one
plot(cust_chaid)