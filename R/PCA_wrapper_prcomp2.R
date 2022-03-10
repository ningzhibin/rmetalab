#' wrapper function for PCA with more options to preprocess data and output plots
#'
#' This wrapper uses prcomp as the method to do pca. This is preferred for omic data, with more features(lines) than samples(columns)
#'
#' @param data_matrix target data matrix for PCA analysis, a long table, with rows as features, while columns as observation/experiments
#' @param data_meta a data.frame with two columns, first as experiment, second as grouping
#' @param inputation logic, if do imputation. default as FALSE,
#' @param Q a value in [0, 1], if do filtering before analysis. default as 0.75, meaning only keep rows with more than 75% present values. Too many missing value will distart PCA analysis
#'
#' @return a list,including direct PCA result and plot
#'
#' @export
#'
#' @examples
#'
#' test_data <- generate_test_data()
#' p <- PCA_wrapper_prcomp2(data_matrix = test_data$matrix, data_meta = test_data$meta, inputation = TRUE, Q = 0.75)
#'
PCA_wrapper_prcomp2 <- function(data_matrix, data_meta, inputation = FALSE, Q = 0.75){

  suppressMessages(install.packages.auto(ggplot2))
  suppressMessages(install.packages.auto(ggfortify))# for autoplot

  data_matrix[is.infinite(data_matrix)] <- NA # replace infinite with missing value for imputation

  # filter out rows with too many missing values
  data_matrix  <- data_matrix[rowSums(is.na(data_matrix)) <= ncol(data_matrix)*(1-Q) , ]


  # do imputations
  if(inputation){
    data_matrix <- rrcovNA::impSeqRob(data_matrix)$x
  }


  # do PCA using prcomp

  PCA_result <- prcomp(t(data_matrix))

  # scree plot
  pca_Scree <- PCA_Screeplot_2(prcomp_out = PCA_result)$Scree.plot

  if(missing(data_meta)){ # if there is no data_meta
    pca_component <- autoplot(PCA_result, label = TRUE )
    pca_component <- pca_component+
      labs(title = "PCA Clustering")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))

    return(list(pca_result = PCA_result,
                pca_component_plot = pca_component,
                pca_scree_plot = pca_Scree
    ))

  }else{
    # order the meta (grouping information)
    data_meta <- data_meta[match(colnames(data_matrix), data_meta[,1]),]
    row.names(data_meta) <- data_meta[,1] # this is only for proper labeling
    colnames(data_meta) <- c("Sample.name", "grouping")

    # check the order
    if(any(colnames(data_matrix) !=  data_meta[,1])){stop("unmatched sample name/colun names")}

    pca_component <- autoplot(PCA_result, data = data_meta, colour = "grouping", label = TRUE)
    pca_component <- pca_component+labs(title = "PCA Clustering")+ theme_bw()+theme(plot.title = element_text(hjust = 0.5))


    pca_kmeans <- PCA_plot_with_ellipse_kmeans_2(prcomp_out = PCA_result, grouping = data_meta)

    pca_3d <- PCA_plot_3d_interactive_3(prcomp_out = PCA_result, grouping = data_meta)

    pca_confidence <- PCA_plot_with_confidence_2(prcomp_out = PCA_result, data_meta = data_meta)


    return(list(pca_result = PCA_result,
                pca_component_plot = pca_component,
                pca_component_plot_kmeans = pca_kmeans,
                pca_component_plot_3d_interactive = pca_3d,
                pca_confidence = pca_confidence,
                pca_scree_plot = pca_Scree
    ))

  }

}
