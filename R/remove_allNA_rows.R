


remove_allNA_rows <- function(df){

  if(any(apply(df,1,function(x)all(is.na(x))))){ # if there is any all NA rows
    df[-which(apply(df,1,function(x)all(is.na(x)))),]
  }else{
    df
  }

  # this function can deal with data.frame and data matrix
  # note that NA has to be NA, otherwise, precess the data first
}
