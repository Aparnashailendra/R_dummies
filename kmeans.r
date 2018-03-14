setwd("C://Users//dell//Desktop//Techdata_assignment//Regression//kmeans")
cust_data <- read.csv("Insurance_Dataset_Clustering_Analysis.csv")
View(cust_data)
cust_data1<-cust_data[,c(2,4,5,7,9,10,13)]
colnames(cust_data1)
head(cust_data1)
install.packages("amap")
K1=kmeans(cust_data1,3, nstart = 200)
K1$centers
K1$size          
K1$withinss          
K1$cluster
cluster_output<-K1$cluster
write.csv(cluster_output,file = "cluster_output.csv")
K1=kmeans(cust_data1,5, nstart = 20)#creating 5 clusters
K1$size
K1$withinss
write.csv(cluster_output,file = "cluster_output1.csv")
