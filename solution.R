# Calling Libraries (and installed the rest)
library(caret)

# Importing the Training and Testing data
training_data <- read.csv(file = 'train_ch.csv')
testing_data <- read.csv(file = 'test_ch.csv')

# Removing the 'X' column from Training and Testing data (the index column)
training_data <- training_data[-1]
testing_data <- testing_data[-1]

# # Graphical Observation
# plot(training_data, main="Training Data")
# plot(testing_data, main="Testing Data")

# Deleting the outliners to improve result, before we had to realize in which column we find the outliners
boxplot(training_data)$out
outliers <- boxplot(training_data$Y, plot=FALSE)$out
training_data_wo <- training_data[-which(training_data$Y %in% outliers),]

# Building our own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalizing the dataset
data_norm_train <- as.data.frame(lapply(training_data_wo[1:9], normalize))
data_norm_test <- as.data.frame(lapply(testing_data, normalize))

# Getting the Target Labels back in the normalized dataset
data_norm_train$Y <- training_data_wo[,10]

# # Checking the observational independence (correlation between variates)
# cor(data_norm_train$v1, data_norm_train$v2)
# cor(data_norm_train$v2, data_norm_train$v3)
# cor(data_norm_train$v3, data_norm_train$v4)
# cor(data_norm_train$v4, data_norm_train$v5)
# cor(data_norm_train$v5, data_norm_train$v6)
# cor(data_norm_train$v6, data_norm_train$v7)
# cor(data_norm_train$v7, data_norm_train$v8)
# cor(data_norm_train$v8, data_norm_train$v9)
# cor(data_norm_train$v9, data_norm_train$v1)

# Plotting the normalized dataset
# boxplot(data_norm_train, main="Preprocessed Training Data")
# boxplot(data_norm_test)

# Linear Regression
fit <- lm(formula = Y ~ v1*v2*v3*v4*v5*v6*v7*v8*v9, data = data_norm_train)
summary(fit)

# # Checking for Homoscedasticity
# par(mfrow=c(2,2))
# plot(fit)
# par(mfrow=c(1,1))

# Predictions of the fitted Linear Regression model
lmPredict <- predict(fit, newdata = data_norm_train)
lm_pred <- predict(fit, newdata = data_norm_test)

# RMSE for Linear Regression
RMSE(lmPredict, data_norm_train$Y)

# # kNN
# k-Fold Cross Validation
set.seed(500)
ctrl <- trainControl(method="repeatedcv", number = 10, repeats = 10)
knnFit <- train(Y ~ v1+v2+v3, data = data_norm_train, method = "knn", trControl = ctrl)

# # Selecting the best combination of model
# ctrl <- trainControl(method = "cv", number = 10)
# step_model <- train(Y ~., data = data_norm_train, method = "leapForward", trControl = ctrl)
# step_model$results
# step_model$bestTune
# summary(step_model$finalModel)

# Predictions of the fitted kNN model
knnPredict <- predict(knnFit, newdata = data_norm_train)
knn_pred <- predict(knnFit, newdata = data_norm_test)

# RMSE for kNN
RMSE(knnPredict, data_norm_train$Y)

# Saving the RData File
save(fit, knn_pred, lm_pred, file = "0063771_ARCHIT_challenge1.Rdata")