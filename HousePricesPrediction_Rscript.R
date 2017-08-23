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
train_rm_na_obser <- knnImputation(train_rm_col_200)

#THE DATA: train_price   train_rm_na_obser_noPrice   imputed_test2



k_fold <- 5
train_rm_na_obser$Id <- sample(1:k_fold, 
                                       nrow(train_rm_na_obser_noPrice), 
                                       replace = TRUE)

prediction <- data.frame()
testsetCopy <- data.frame()

for(i in 1:k_fold){
    #useful in other place
    train_set <- train_rm_na_obser[!(train_rm_na_obser$Id==i),]
    test_set <- train_rm_na_obser[train_rm_na_obser$Id==i,]
    test_set$SalePrice <- NULL
    
    rf_model <- randomForest(SalePrice ~ .,
                             data = train_set)
    
    #predict
    temp <- as.data.frame(predict(rf_model, test_set))
    prediction <- rbind(prediction, temp)
    
    #order the label of test data
    temp2 <- as.data.frame(train_rm_na_obser$SalePrice[train_rm_na_obser$Id==i])
    testsetCopy <- rbind(testsetCopy, temp2)
    
    #error
    err <- mse(prediction, testsetCopy)
}