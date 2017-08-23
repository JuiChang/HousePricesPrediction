#cv reference: https://www.analyticsvidhya.com/blog/2015/11/improve-model-performance-cross-validation-in-python-r/
library("Matrix")
library("xgboost")
library("dplyr")
library("DMwR")
library("hydroGOF")
library('randomForest')

train <- read.csv('../input/House-Prices/train.csv', stringsAsFactors = T)
test <- read.csv('../input/House-Prices/test.csv', stringsAsFactors = T)


#remove the columns which #NA is more than 200
#because some of the columns has lots of NA (more than 1000)
k <- apply(train, 2, is.na)
l <- apply(k, 2, sum)
train_rm_col_200 <- train[l<=200]


train_rm_col_200_2 <- train_rm_col_200
train_rm_col_200_2$SalePrice <- NULL
test2 <- test[colnames(test) %in% colnames(train_rm_col_200_2)]

#Imputation for the test data
imputed_test2 <- knnImputation(test2) 

#Imputation for the train data
imputed_train <- knnImputation(train_rm_col_200)

#THE DATA:   imputed_train   imputed_test2

    
rf_model <- randomForest(SalePrice ~ .,
                        data = imputed_train)

###############################debug:########################################
#bug:Error in predict.randomForest(rf_model, imputed_test2) : 
#Type of predictors in new data do not match that of the training data.

#reference:https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match

#train_factorCols_logi <- sapply(imputed_train, is.factor)
#train_factor_df <- data.frame(imputed_train[train_factorCols_logi])
#train_factor_levels <- sapply(train_factor_df, levels)

#factor_count <- 0
#for(i in 1:ncol(imputed_test2)){
#    if(is.factor(imputed_test2[i])){
#        factor_count <- factor_count + 1
#        levels(imputed_test2[i]) <- train_factor_levels[factor_count]
#    }
#}
#######above didn't work#######

imputed_train_2 <- imputed_train
imputed_train_2$SalePrice <- NULL
imputed_test2 <- rbind(imputed_train_2[1, ], imputed_test2)
imputed_test2 <- imputed_test2[-1,]
################################################################################

y_pred <- predict(rf_model, imputed_test2)

y_pred_df <- data.frame(y_pred)
write.csv(y_pred_df, "submission_3.csv")