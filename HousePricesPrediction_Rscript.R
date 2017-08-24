#cv reference: https://www.analyticsvidhya.com/blog/2015/11/improve-model-performance-cross-validation-in-python-r/
library("Matrix")
library("xgboost")
library("dplyr")
library("DMwR")
library("hydroGOF")
library('randomForest')
source("myFunctions_Rscript.R")

train <- read.csv('../input/House-Prices/train.csv', stringsAsFactors = T)
test <- read.csv('../input/House-Prices/test.csv', stringsAsFactors = T)


#remove the columns which #NA is more than 200
#because some of the columns has lots of NA (more than 1000)
k <- apply(train, 2, is.na)
l <- apply(k, 2, sum)
train_rm_col_200 <- train[l<=200]


train_rm_col_200_2 <- train_rm_col_200
train_rm_col_200_2$SalePrice <- NULL
#test2 <- test[colnames(test) %in% colnames(train_rm_col_200_2)]

#Imputation for the test data
#imputed_test2 <- knnImputation(test2) 

#Imputation for the train data
imputed_train <- knnImputation(train_rm_col_200)

#THE DATA:   imputed_train   imputed_test2
imputed_train_drop_price <- imputed_train
imputed_train_drop_price$SalePrice <- NULL
#df_all <- rbind(imputed_train_drop_price, imputed_test2)

############################DO Feature Engineering Here###########################
#is_factor_col_logi <- sapply(df_all, is.factor)
#is_factor_col <- data.frame(df_all[is_factor_col_logi])
#factorCol_levelsList <-list(sapply(is_factor_col, levels))
#levels_isQual_logi <- sapply(factorCol_levelsList, 
#                             function(x) x==list(levels(factorCol_levelsList$HeatingQC)))
#above: incomplete

col_cor_list <- lapply(imputed_train_drop_price, 
                       function(x) cor.with.label(x, imputed_train$SalePrice))
imputed_trainDP_rmLCC <- rm.lowCorCols(col_cor_list, imputed_train_drop_price,
                                       0.2, -0.2)

#imputed_trainDP_rmLCC <- data.frame(lapply(imputed_train_drop_price,  
#                                       ##sapply changed the factor col to num...
#                                                function(x){
#                                                    if(col_cor_list[[i]][1] > 0.2 |
#                                                       col_cor_list[[i]][1] < -0.2){
#                                                        return(x)}
#                                                    else print("LALA")
#                                                }))
#above: incomplete
##################################################################################
test2 <- test[colnames(test) %in% colnames(imputed_trainDP_rmLCC)]

#Imputation for the test data
imputed_test2 <- knnImputation(test2) 

df_all <- rbind(imputed_trainDP_rmLCC, imputed_test2)


df_all_sparse_matrix <- sparse.model.matrix(~., data=df_all)
train_sparse_matrix_2 <- df_all_sparse_matrix[imputed_train$Id,]
test_sparse_matrix_2 <- df_all_sparse_matrix[(nrow(imputed_train)+1):(nrow(df_all)),] #bug here

xgb <- xgboost(data = data.matrix(train_sparse_matrix_2), 
               label = imputed_train$SalePrice,
               objective="reg:linear",
               nround=25)



y_pred <- predict(xgb, test_sparse_matrix_2)

y_pred_df <- data.frame(y_pred)
write.csv(y_pred_df, "submission_5.csv")