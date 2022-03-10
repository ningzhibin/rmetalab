#' Generate a fake metaproteomic/proteomic data matrix, with grouping information
#'
#' @param seed
#'
#' @return a list with a matrix, of 200 rows and 30 columns, and a meta table, with two columns, sample name and grouping
#'
#' @export
#'
#' @examples
#'
#' test_data <- generate_test_data()
#' test_data$matrix
#' test_data$meta
#'
#'
generate_test_data <- function(seed = 1981){

  set.seed(seed)

  matrix_1 <- matrix(runif(2000), nrow = 200)
  matrix_2 <- matrix(runif(2000)+ runif(2000), nrow = 200)
  matrix_3 <- matrix(runif(2000)+ 2*runif(2000), nrow = 200)


  colnames(matrix_1) <- paste("sample_A", 1:10, sep = "_")
  colnames(matrix_2) <- paste("sample_B", 1:10, sep = "_")
  colnames(matrix_3) <- paste("sample_C", 1:10, sep = "_")



  matrix <- cbind(matrix_1,matrix_2, matrix_3)

  rownames(matrix) <- paste("protein", 1:200, sep = "_")
  #colnames(matrix_test) <- paste("sample", 1:30, sep = "_")

  factor_1 <- c(rep("A",10),rep("B",10),rep("C",10))

  meta <- data.frame(samplename = colnames(matrix),
                          CellType = factor_1,
                          TimePoint = 1:10)

  # Randomize the order
  matrix <- matrix[,sample(1:ncol(matrix))]
  meta <- meta[sample(1:nrow(meta)),]
  rownames(meta) <- NULL

  return(list(matrix =matrix,
              meta =  meta))

}



