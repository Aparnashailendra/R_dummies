setwd("E:\\kaggle\\housing data")
melbourne_data <- read.csv("E:\\kaggle\\melb_data.csv")
summary(melbourne_data)
install.packages("tidyverse")
library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests
# train a decision tree based on our dataset 
fit <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea +
               YearBuilt + Lattitude + Longtitude, data = melbourne_data)
# plot our regression tree 
plot(fit, uniform=TRUE)
# add text labels & make them 60% as big as they are by default
text(fit, cex=.6)
print("Making predictions for the following 5 houses:")
print(head(melbourne_data))

print("The predictions are")
print(predict(fit, head(melbourne_data)))

print("Actual price")
print(head(melbourne_data$Price))

# package with the mae function
library(modelr)

# get the mean average error for our model
mae(model = fit, data = melbourne_data)
# split our data so that 30% is in the test set and 70% is in the training set
splitData <- resample_partition(melbourne_data, c(test = 0.3, train = 0.7))

# how many cases are in test & training set? 
lapply(splitData, dim)


# fit a new model to our training set
fit2 <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea +
                YearBuilt + Lattitude + Longtitude, data = splitData$train)

# get the mean average error for our new model, based on our test data
mae(model = fit2, data = splitData$test)


# a function to get the maximum average error for a given max depth. You should pass in
# the target as the name of the target column and the predictors as vector where
# each item in the vector is the name of the column
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  
  # turn the predictors & target into a formula to pass to rpart()
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target,"~",predictors,sep = ""))
  
  # build our model
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  # get the mae
  mae <- mae(model, testing_data)
  return(mae)
}

# target & predictors to feed into our formula
target <- "Price"
predictors <-  c("Rooms","Bathroom","Landsize","BuildingArea",
                 "YearBuilt","Lattitude","Longtitude")

# get the MAE for maxdepths between 1 & 10
for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors,
                 training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
}