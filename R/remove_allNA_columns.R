
remove_allNA_columns <- function(df){
  if(any(apply(df,2,function(x)all(is.na(x))))){ # if there is any all NA columns
    df[,-which(apply(df,2,function(x)all(is.na(x))))]
  }else{
    df
  }
  # this function can deal with data.frame and data matrix
  # note that NA has to be NA, otherwise, precess the data first
}
