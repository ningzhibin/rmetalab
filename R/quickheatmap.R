#' simple wrapper of heatmap.2, for quick and easy heatmap plot from data matrix and meta info, with only limited parameters exposed
#'
#' 20220216 reformat it into rmetalab
#' 20191002 add a line to replace the infinite values, heatmap.2 can deal with NA, but not infinte values
#'
#'
#' @param matrix a data.frame/matrix
#' @param scale The default is "none", scaling method, options of c("none","row", "column")
#' @param column_cluster Boolean, Switch to do column clustering of not

#' @param column_meta a data.frame with 2 columns, 1st as column variables, 2nd as grouping, will map to colors
#' @param row_meta a data.frame with 2 columns, 1st as row variables, 2nd as grouping, will map to colors
#'
#' @param color one of redblue,bluered, greenred, redgreen
#'
#' @return
#'
#' @examples
#' test_data <- generate_test_data()
#' quickheatmap(test_data$matrix)
#' quickheatmap(test_data$matrix, column_cluster = FALSE)
#' quickheatmap(test_data$matrix, column_meta = test_data$meta[,1:2])
#' quickheatmap(test_data$matrix, column_meta = test_data$meta[,1:2], column_cluster = FALSE) # used to check the pattern without column clustering
#'
#' @export
#'

quickheatmap <- function(matrix,
                         column_cluster = TRUE,
                         column_meta = NULL ,
                         row_meta = NULL,
                         scale = "row",
                         color = bluered ){

  library(gplots)#

  # ColSideColors cannot be setup to NULL, therefore need condition to switch

  matrix[is.infinite(matrix)] <- NA # in case there is infinite values


  # get the color code for the grouping from the meta table

  if(!is.null(row_meta)){
    row_meta <- row_meta[match(rownames(matrix),row_meta[,1]),]
    row_color_labeling <-factor2color(row_meta[,2])
  }

  if(!is.null(column_meta)){
    # do not touch the order of the matrix, only re-order the meta
    column_meta <- column_meta[match(colnames(matrix),column_meta[,1]),] # reorder meta table to be consistent to the data matrix
    column_color_labeling <-factor2color(column_meta[,2])

  }

  if(column_cluster){
    dendrogram = "both"
    Colv =TRUE
  }else{
    dendrogram = "row"
    Colv = NULL
  }

  # start plotting
  if(is.null(row_meta) & is.null(column_meta)){

    p <-  gplots::heatmap.2(matrix,
                            scale = scale,
                            Colv  = Colv,
                            dendrogram = dendrogram,
                            keysize = 1.5,
                            col=color,
                            density.info = "none",
                            trace = "none",
                            key.title  = "",
                            key.xlab = "",
                            key.ylab = "")



  }else if(!is.null(column_meta) & is.null(row_meta)){
    # if only column grouping information provided
    p <-  gplots::heatmap.2(matrix,
                            ColSideColors = column_color_labeling,
                            scale = scale,
                            Colv  = Colv,
                            dendrogram = dendrogram,
                            col=color,
                            keysize = 1.5,
                            density.info = "none",
                            trace = "none",
                            key.title  = "",
                            key.xlab = "",
                            key.ylab = "")




  }else if(is.null(column_meta)  & !is.null(row_meta)){
    # if only row grouping information provided
    p <-  gplots::heatmap.2(matrix,
                            RowSideColors = row_color_labeling,
                            scale = scale,
                            Colv  = Colv,
                            dendrogram = dendrogram,
                            col=color,
                            keysize = 1.5,
                            density.info = "none",
                            trace = "none",
                            key.title  = "",
                            key.xlab = "",
                            key.ylab = "" )
  }else{

    p <-  gplots::heatmap.2(matrix,
                            RowSideColors = row_color_labeling,
                            ColSideColors = column_color_labeling,
                            scale = scale,
                            Colv  = Colv,
                            dendrogram = dendrogram,
                            col=color,
                            keysize = 1.5,
                            density.info = "none",
                            trace = "none",
                            key.title  = "",
                            key.xlab = "",
                            key.ylab = "" )
  }

  return(p)
}


