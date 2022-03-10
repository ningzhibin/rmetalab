
#' plot beta divesity using PCoA
#'
#' @param count_species
#' @param meta_table
#'
#' @return
#' @export
#'
#' @examples
plot_beta_diversity <- function(count_species, meta_table =  NULL){
  # input as count data.frame, row as experiment, column as taxons
  # Bray–Curtis dissimilarity
  # if number of experiment is only 3, 2d plot can only be produced,
  # while more than 3, 3d plot can be produced
  dist_matrix_bray<- vegan::vegdist(t(count_species),method="bray") %>% as.matrix

  if(is.null(meta_table)){

    if(nrow(dist_matrix_bray) >3){ # 3d plot
      PCoA.bray <- cmdscale(dist_matrix_bray, k=3, eig = TRUE)$points  %>%
        as.data.frame %>%
        rownames_to_column(var = "Experiment")
      p <- plot_ly(PCoA.bray, x = ~V1, y = ~V2, z = ~V3, color = ~Experiment,
                   colors = "Dark2") %>%
        add_markers() %>%
        add_text(text = ~Experiment) %>%
        hide_legend() %>%
        layout(title = "Bray-Curtis PCoA",
               scene = list(xaxis = list(title = "PCoA 1"),
                            yaxis = list(title = "PCoA 2"),
                            zaxis = list(title = "PCoA 3")))

    }else if (nrow(data_matrix_t) ==3){ # 2d plot
      PCoA.bray <- cmdscale(dist_matrix_bray, k=2, eig = TRUE)$points  %>%
        as.data.frame %>%
        rownames_to_column(var = "Experiment")

      p <- plot_ly(PCoA.bray, x = ~V1, y = ~V2, color = ~Experiment,
                   colors = "Dark2") %>%
        add_markers() %>%
        add_text(text = ~Experiment) %>%
        hide_legend() %>%
        layout(title = "Bray-Curtis PCoA",
               xaxis = list(title = "PCoA 1"),
               yaxis = list(title = "PCoA 2"))
    }

    plot_list <- list(plot_beta = p)

  }else{

    if(nrow(dist_matrix_bray) >3){
      PCoA.bray <- cmdscale(dist_matrix_bray, k=3, eig = TRUE)$points  %>% as.data.frame

      plot_list <- lapply(as.list(colnames(meta_table)[-1]),function(x){

        PCoA.bray <- PCoA.bray %>% cbind(Group = meta_table[[x]], .) %>% rownames_to_column(var = "Experiment")

        plot_ly(PCoA.bray, x = ~V1, y = ~V2, z = ~V3, color = ~ Group ,
                colors = "Dark2") %>%
          add_markers() %>%
          add_text(text = ~Experiment) %>%
          layout(title = "Bray-Curtis PCoA",
                 scene = list(xaxis = list(title = "PCoA 1"),
                              yaxis = list(title = "PCoA 2"),
                              zaxis = list(title = "PCoA 3")))

      })

      names(plot_list)  <- colnames(meta_table)[-1]

    }else if (nrow(data_matrix_t) ==3){

      PCoA.bray <- cmdscale(dist_matrix_bray, k=2, eig = TRUE)$points  %>%
        as.data.frame

      plot_list <- lapply(as.list(colnames(meta_table)[-1]),function(x){

        PCoA.bray_plot <- PCoA.bray %>% cbind(Group = meta_table[[x]], .) %>% rownames_to_column(var = "Experiment")

        plot_ly(PCoA.bray_plot, x = ~V1, y = ~V2, color = ~Group,
                colors = "Dark2") %>%
          add_markers() %>%
          add_text(text = ~Experiment) %>%
          hide_legend() %>%
          layout(title = "Bray-Curtis PCoA",
                 xaxis = list(title = "PCoA 1"),
                 yaxis = list(title = "PCoA 2"))

      })

      names(plot_list)  <- colnames(meta_table)[-1]

    }
    PCoA.bray <-cbind(meta_table,PCoA.bray) # only for output
  }
  return(list(plot_list =plot_list,
              PCoA.bray =PCoA.bray))

}

# 20220307 debug, add plot_list for no grouping data
