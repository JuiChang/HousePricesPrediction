cor.with.label <- function(x.numlist, y.numlist){
    if(is.numeric(x.numlist)){
        return(cor(y.numlist, x.numlist))
    }
    return(2)
}

rm.lowCorCols <- function(col_cor_list, df, h_limit, l_limit){
    for(i in 1:length(col_cor_list)){
        if(col_cor_list[i] < h_limit & col_cor_list[i] > l_limit){
            #df[i] <- NULL
            #the above command lead to bug when 
            #compiling HousePricesPrediction_Rscript,R
            #"Error in `[<-.data.frame`(`*tmp*`, i, value = NULL) : 
            #new columns would leave holes after existing columns"
            #so I change the method of "dropping col from a dataframe below
            #reference:  https://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
            df <- df[ -c(i) ]
        }
    }
    return(df)
}