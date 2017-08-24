library("Matrix")
library("xgboost")
library("dplyr")
library("DMwR")
library("hydroGOF")
library('randomForest')
source("myFunctions_Rscript.R")

train <- read.csv('../input/House-Prices/train.csv', stringsAsFactors = T)


#remove the columns which #NA is more than 200
#because some of the columns has lots of NA (more than 1000)
k <- apply(train, 2, is.na)
l <- apply(k, 2, sum)
train_rm_col_200 <- train[l<=200]

#Imputation for the train data
imputed_train <- knnImputation(train_rm_col_200)


imputed_train_drop_price <- imputed_train
imputed_train_drop_price$SalePrice <- NULL


col_cor_list <- lapply(imputed_train_drop_price, 
                       function(x) cor.with.label(x, imputed_train$SalePrice))
imputed_trainDP_rmLCC <- rm.lowCorCols(col_cor_list, imputed_train_drop_price,
                                       0.2, -0.2)
#DP: drop SalePrice ;  rmLCC: remove low correlation(with the labels) columns



k_fold <- 5
imputed_train$Id <- sample(1:k_fold, 
                           nrow(imputed_train), 
                           replace = TRUE)

prediction <- data.frame()
testsetCopy <- data.frame()

for(i in 1:k_fold){
    #useful in other place
    #train_set <- train_rm_na_obser_noPrice[!(train_rm_na_obser_noPrice$Id==i),]
    #test_set <- train_rm_na_obser_noPrice[train_rm_na_obser_noPrice$Id==i,]
    
    #for xgboost specifically
    df_all_sparse_matrix <- sparse.model.matrix(~., data=imputed_trainDP_rmLCC)
    train_sparse_matrix <- df_all_sparse_matrix[
                                imputed_train$Id!=i,]
    test_sparse_matrix <- df_all_sparse_matrix[
                                imputed_train$Id==i,]
    
    xgb <- xgboost(data = train_sparse_matrix, 
                   label = imputed_train$SalePrice[imputed_train$Id!=i],
                   #booster="gblinear", #default is "gbtree"
                   objective="reg:linear",
                   nround=25)
    
    #predict
    temp <- as.data.frame(predict(xgb, test_sparse_matrix))
    prediction <- rbind(prediction, temp)
    
    #order the label of test data
    temp2 <- as.data.frame(imputed_train$SalePrice[imputed_train$Id==i])
    testsetCopy <- rbind(testsetCopy, temp2)
}

#error
err <- mse(prediction, testsetCopy)