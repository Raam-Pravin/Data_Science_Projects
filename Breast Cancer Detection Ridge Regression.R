
library(corrplot)
library(Hmisc)
library(ggplot2)
library(psych)
library(GGally)
library(vioplot)
library(DescTools)
library(leaps)
library(glmnet)

bcancer <- read.csv("C:/Users/Raamp/Downloads/data.csv")
bcancer$diagnosis[bcancer$diagnosis=="M"] <- 1
bcancer$diagnosis[bcancer$diagnosis=="B"] <- 0
bcancer$diagnosis<- as.numeric(bcancer$diagnosis)

bcancer <- bcancer[,c(2:32)]


set.seed(6483)
lregression_indexes <- sample(2, nrow(bcancer), replace = TRUE, prob = c(0.8,0.2))
lregression_train <- bcancer[lregression_indexes==1,]
lregression_test <- bcancer[lregression_indexes==2,]

# Separate predictors and target variable
X_train <- lregression_train[,c(2:31)]  # Exclude the diagnosis column
y_train <- lregression_train$diagnosis

lambda_seq <- 10^seq(2, -2, by = -.1)
# Performing the ridge regression
ridge_model <- glmnet(as.matrix(X_train), y_train, alpha = 0, 
                         lambda = lambda_seq)
summary(ridge_model)

ridge_cv <- cv.glmnet(as.matrix(X_train), y_train, alpha = 0)
best_lambda <- ridge_cv$lambda.min
best_lambda

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

best_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda)

coef(best_ridge)

X_test <- lregression_test[,c(2:31)]
X_test <- as.matrix(X_test)
y_test <- lregression_test$diagnosis
y_test <- as.matrix(y_test)

pred <- predict(best_ridge, s = best_lambda, newx = X_test)
length(pred)
library(caret)
testing_predictions <- ifelse(pred > 0.5, 1, 0)
testing_predictions <- factor(testing_predictions)
lregression_test$diagnosis <- factor(lregression_test$diagnosis)


confusion_matrix <- confusionMatrix(testing_predictions, lregression_test$diagnosis)
confusion_matrix


