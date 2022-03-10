#' Biological ternary plot from three rows of the matrix
#'
#' This is a wrapper of ggern function in ggtern package. The wrapper makes it easier to plot matrix data with grouping information.
#' See the details for more information, and run the example for a sample plot to have a look at what a ternary plot look like
#'
#'
#' @param data_matrix a two dimentional data matrix, with rows as annation/function/taxon, while columns are samples
#' @param data_meta a dataframe with sample-group matching
#' @param three_points could be "top3", or a vector of row index, or a vector of row names. If vector, the length must be 3. see examples
#'
#' @return a data.frame of matrix_selected and ggplot2 object of ternary.plot
#'
#' @seealso \code{\link{ggtern}}
#'
#' @examples
#' #data preparation
#'
#'  matrix_test <- matrix(runif(2000), nrow = 20)
#'  rownames(matrix_test) <- paste("COG", 1:20, sep = "_")
#'  colnames(matrix_test) <- paste("sample", 1:100, sep = "_")
#'  factor_test <- sample(LETTERS[1:3], 100, replace = TRUE)
#'  meta_test <- data.frame(samplename = colnames(matrix_test), grouping = factor_test)
#'
#' #Plot in three ways
#'
#' t <- ternary_plot_matrix(data_matrix = matrix_test, data_meta = meta_test, three_points = c("COG_1", "COG_2", "COG_3"))
#' t$ternary.plot
#' t <- ternary_plot_matrix(data_matrix = matrix_test, data_meta = meta_test, three_points = "top3")
#' t$ternary.plot
#' t <- ternary_plot_matrix(data_matrix = matrix_test, data_meta = meta_test, three_points = c(2,4,5))
#' t$ternary.plot
#'
#'
#' @export



ternary_plot_matrix <- function(data_matrix, data_meta, three_points){

  suppressMessages(install.packages.auto(ggtern))

  mode(data_matrix) <-"numeric" # in case mode is character

  if(length(three_points) ==1 && three_points == "top3"){
    three_points = rownames(data_matrix)[1:3]
  } else if(is.numeric(three_points)){
    # check if some of the location out of boundary(nrow)
    if(any(unlist((lapply(three_points, function(x) x>20))))){
      stop("out of boundary")
    }else{
      three_points = rownames(data_matrix)[three_points]
    }
  }else if(is.character(three_points)){
    index <- match(three_points, rownames(data_matrix))
    if( any(unlist(lapply(index, is.na))) ) {
      stop("item_name Not found, please input the right one")
    }
  }

  select_3 <- as.data.frame(t(data_matrix[which(rownames(data_matrix) %in% three_points),]))
  # select_3 <- as.data.frame(scale(select_3), scale = FALSE) # rescale

  # reorder the data_meta, in case there is mis-match of the column names
  data_meta_Reorder <- data_meta[match(rownames(select_3), data_meta[,1]),]

  select_3$Group <- data_meta_Reorder[,2]

  colnames(select_3)<-c("x","y","z", "Group")
  p <- ggtern(data=select_3,aes(x,y,z,color=Group)) + geom_point() + labs(x=three_points[1],y=three_points[2],z=three_points[3],title="Ternary Plot")


  return(list(ternary.plot = p,
              matrix_selected = select_3
  ))

}




# ternary_plot <- function(data_matrix, data_meta, three_points){
#
#   install.packages.auto(ggtern)
#
#   select_3 <- as.data.frame(t(data_matrix[which(rownames(data_matrix) %in% three_points),]))
#   #select_3 <- as.data.frame(scale(select_3), scale = FALSE) # rescore
#
#   index <- match(rownames(select_3), data_meta[,1])
#   data_meta_Reorder <- data_meta[index,]
#
#   select_3[,4] <- data_meta_Reorder[,2]
#   #select_3_hits[,5] <- data_meta_Reorder[,1]
#
#
#   colnames(select_3)<-c("x","y","z", "Group")
#   p<- ggtern(data=select_3,aes(x,y,z,color=Group)) +geom_point() +labs(x=three_points[1],y=three_points[2],z=three_points[3],title="Ternary Plot")
#
#
#
#   return(list(ternary.plot = p,
#               matrix_selected = select_3
#   ))
#
# }



