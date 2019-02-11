library(tidyverse)
library(rpart)
library(randomForest)
library(modelr)
library(glue)

#loading train data set
iowa_data <- read_csv("train.csv")

#change to factor
iowa_data$Condition1 <- as.factor(iowa_data$Condition1)

#see the data 
glimpse(iowa_data)
summary(iowa_data)
colnames(iowa_data)   #also can use names(iowa_data)

#building the first model
fit <- rpart(SalePrice ~ LotArea + YearBuilt + Condition1 + 
               FullBath + BedroomAbvGr + TotRmsAbvGrd, data = iowa_data)

#use model to predict the SalePrice
prediction <- predict(fit, iowa_data)

# plot our regression tree 
plot(fit, uniform=TRUE)
# add text labels & make them 60% as big as they are by default
text(fit, cex=.6)

#compare the prices with prediction prices
print(head(iowa_data))

print(head(iowa_data$SalePrice))

print(head(prediction))

#checking MAE (mean average error)
mae(model = fit, data = iowa_data)

#introduce train & test sets
splitData <- resample_partition(iowa_data, c(test = 0.3, train = 0.7))

#checking sizes of both sets
lapply(splitData, dim)

fit2 <- rpart(SalePrice ~ LotArea + YearBuilt + Condition1 + 
                FullBath + BedroomAbvGr + TotRmsAbvGrd, data = splitData$train)

mae(fit2, data = splitData$test)

# iowa_data <- na.omit(iowa_data) #dropping the data with NA's, one of the way of dealing with missing values

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

#use for loop to find out MAE for different depth
target <- "SalePrice"
predictors <- c("LotArea", "YearBuilt", "Condition1", 
                "FullBath", "BedroomAbvGr", "TotRmsAbvGrd")

for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors,
                 training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
}


#check MAE (mean average error) for a randomForest
fitRandomForest <- randomForest(SalePrice ~ LotArea + YearBuilt + Condition1 + 
                                  FullBath + BedroomAbvGr + TotRmsAbvGrd, data = splitData$train)

# Check out the MAE. Did you see an improvement over your original model?
mae(model = fitRandomForest, data = splitData$test)