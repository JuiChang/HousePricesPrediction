library("Matrix")
library("xgboost")
library("dplyr")
library("DMwR")
library("hydroGOF")

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


#remove the observations of training data which include NA
train_rm_na_obser <- train_rm_col_200[complete.cases(train_rm_col_200), ]

train_price <- data.frame(Id=train_rm_na_obser$SalePrice,
                          Price=train_rm_na_obser$SalePrice)
train_rm_na_obser_noPrice <- train_rm_na_obser
train_rm_na_obser_noPrice$SalePrice <- NULL

#THE DATA: train_price   train_rm_na_obser_noPrice   imputed_test2

#the variables of the two sparse matrices(from train and test) was different
#so I merge the two data frame and convert to a sparse matrix, then split it back
#to train part and test part.
#df_all <- rbind(train_rm_na_obser_noPrice, imputed_test2)
#df_all_sparse_matrix <- sparse.model.matrix(~., data=df_all)
#train_sparse_matrix_2 <- df_all_sparse_matrix[train_rm_na_obser$Id,]
#test_sparse_matrix_2 <- df_all_sparse_matrix[
#                            !((df_all$Id)%in%(train_rm_na_obser$Id)),]


k_fold <- 5
train_rm_na_obser_noPrice$Id <- sample(1:k_fold, 
                                       nrow(train_rm_na_obser_noPrice), 
                                       replace = TRUE)

train_price$Id <- train_rm_na_obser_noPrice$Id

prediction <- data.frame()
testsetCopy <- data.frame()

for(i in 1:k_fold){
    #useful in other place
    #train_set <- train_rm_na_obser_noPrice[!(train_rm_na_obser_noPrice$Id==i),]
    #test_set <- train_rm_na_obser_noPrice[train_rm_na_obser_noPrice$Id==i,]
    
    #for xgboost specifically
    #df_all <- rbind(train_set, test_set)
    df_all_sparse_matrix <- sparse.model.matrix(~., data=train_rm_na_obser_noPrice)
    train_sparse_matrix_2 <- df_all_sparse_matrix[
        !(train_rm_na_obser_noPrice$Id==i),]
    test_sparse_matrix_2 <- df_all_sparse_matrix[
        train_rm_na_obser_noPrice$Id==i,]
    
    #form the model
    xgb <- xgboost(data = data.matrix(train_sparse_matrix_2), 
                   label = train_price$Price[train_price$Id!=i],
                   #booster="gblinear", #default is "gbtree"
                   objective="reg:linear",
                   nround=25)
    
    #predict
    temp <- as.data.frame(predict(xgb, test_sparse_matrix_2))
    prediction <- rbind(prediction, temp)
    
    #order the label of test data
    temp2 <- as.data.frame(train_price$Price[train_price$Id==i])
    testsetCopy <- rbind(testsetCopy, temp2)
    
    #error
    err <- mse(prediction, testsetCopy)
}


#form the model
#xgb <- xgboost(data = data.matrix(train_sparse_matrix_2), 
#               label = train_price,
#               objective="reg:linear",
#               nround=25)

#predict
#y_pred <- predict(xgb, test_sparse_matrix_2)

#y_pred_df <- data.frame(y_pred)
#write.csv(y_pred_df, "submission_2.csv")