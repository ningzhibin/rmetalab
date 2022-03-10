

#' boxPlot a row in a matrix
#'
#' @param row index of the row to plot
#' @param data_matrix input of the data matrix
#' @param data_meta optional, a data.frame of meta, first column as sample name corresponding to the column name of the data matrix, second column as grouping inforation
#' @param plot_type string, either  violin or boxplot
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#'
#' # prepare data
#' t <- generate_test_data()
#'
#' # plot a single row
#' plot_box_matrix_byrow(row = 1, data_matrix = t$matrix, data_meta = t$meta, plot_type = "violin")
#'
#' # plot multiple rows into a list
#' plot_multiple_rows <- lapply(1:10, plot_box_matrix_byrow, data = t$matrix, data_meta = t$meta)
#'
#'
plot_box_matrix_byrow <- function(row = 1, data_matrix, data_meta = NULL, plot_type = "violin"){


  if(is.numeric(row)){
    index <- row
    if(index > nrow(data_matrix) | index <=0) stop("row number is out of range (numer of column number), please input the right one")

  }else if(is.character(row)){
    index <- match(row, colnames(data_matrix_tranverse))
    if(is.na(index)) stop("item_name Not found, please input the right one")
  }

  title = rownames(data_matrix)[index]


  if(is.null(data_meta)){
    grouping =1
  }else{
    # reorder the meta
    data_meta <- data_meta[match(colnames(data_matrix), data_meta[,1]),]
    # check the seqeunce
    if(!all(colnames(data_matrix) ==  data_meta[,1])){stop("unmatched")}

    grouping = data_meta[,2]

  }

  plot <- plot_box_vector(vector = data_matrix[index,], factor = grouping, plot_type = plot_type, title = title)
  return(plot)
}
