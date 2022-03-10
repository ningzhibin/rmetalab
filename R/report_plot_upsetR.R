#' generate a plot list with upsetR, to compare with columns, groups if meta provided
#'
#' This function is for rmarkdonwn report purpose. If meta/grouping information provided, it will generate all comparisons for all groupings, otherwise the plot for all across the sample/columns
#'
#' @param df_intensity a data.frame of intensity matrix, with row names defined
#' @param meta_table a meta data with right order of grouping, no order check inside the function
#'
#' @return a plot list
#' @export
#'
#' @examples
report_plot_upsetR <- function(df_intensity, meta_table = NULL){

  if(!is.null(meta_table)){
    plot_list <- lapply(as.list(colnames(meta_table)[-1]),function(x){
      # summary
      df_intensity_for_upset <- df_intensity %>%  t %>%
        aggregate(x = ., by =list(meta =meta_table[[x]]), FUN = "sum") %>%
        as.data.frame %>%
        column_to_rownames(var = "meta" ) %>% t
      # binary
      df_intensity_for_upset <- as.matrix((df_intensity_for_upset > 0) + 0) %>%
        as.data.frame %>%
        rownames_to_column(var = "name")
      # plot
      upset(df_intensity_for_upset, order.by = "freq", number.angles = 30, point.size = 4, line.size = 1,
            text.scale = c(1.5, 1.3, 1.5, 1.5, 1.5, 1.75), mb.ratio = c(0.6, 0.4))
    })

    names(plot_list)  <- colnames(meta_table)[-1]

  }else{
    # binary
    df_intensity_for_upset <- as.matrix((df_intensity > 0) + 0) %>%
      as.data.frame %>%
      rownames_to_column(var = "name")
    # plot
    plot_experiment <- UpSetR::upset(df_intensity_for_upset,nsets = ncol(df_intensity), order.by = "freq", number.angles = 30, point.size = 4, line.size = 1,
                             text.scale = c(1.5, 1.3, 1.5, 1.5, 1.5, 1.75), mb.ratio = c(0.6, 0.4))

    plot_list <- list( plot_experiment = plot_experiment)
  }

  return(plot_list)

}
