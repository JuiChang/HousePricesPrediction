library("Matrix")
library("xgboost")
library("dplyr")
library("DMwR")

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

##############################################################No use here:
col_has_na <- apply(test2, 2, function(x) sum(is.na(x)))
#test3 includes the cols of test2 which have NA
test3 <- test2[(col_has_na!=0)]

#there are two type of cols in test3, factor and int,
#get the factor cols
#reference:https://stackoverflow.com/questions/5863097/selecting-only-numeric-columns-from-a-data-frame
factor_has_na <- sapply(test3, is.factor)
test3_factor <- test3[factor_has_na]
#get the int cols
int_has_na <- sapply(test3, is.integer)
test3_int <- test3[int_has_na]
##########################################################################


#Imputation for the test data
imputed_test2 <- knnImputation(test2) 


#remove the observations of training data which include NA
train_rm_na_obser <- train_rm_col_200[complete.cases(train_rm_col_200), ]

train_rm_na_obser_price <- train_rm_na_obser$SalePrice
train_rm_na_obser$SalePrice <- NULL

#the variables of the two sparse matrices(from train and test) was different
#so I merge the two data frame and convert to a sparse matrix, then split it back
#to train part and test part.
df_all <- rbind(train_rm_na_obser, imputed_test2)
df_all_sparse_matrix <- sparse.model.matrix(~., data=df_all)
train_sparse_matrix_2 <- df_all_sparse_matrix[train_rm_na_obser$Id,]
test_sparse_matrix_2 <- df_all_sparse_matrix[1339:2797,]


#form the model
xgb <- xgboost(data = data.matrix(train_sparse_matrix_2), 
               label = train_rm_na_obser_price,
               objective="reg:linear",
               nround=25)

#predict
y_pred <- predict(xgb, test_sparse_matrix_2)

y_pred_df <- data.frame(y_pred)
write.csv(y_pred_df, "submission_2.csv")

#y_pred2 <- predict(xgb, data.matrix(train_sparse_matrix_2))