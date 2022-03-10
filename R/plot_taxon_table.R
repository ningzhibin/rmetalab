
#' Title
#'
#' @param taxon_table
#' @param meta_table
#' @param ylabel
#'
#' @return
#' @export
#'
#' @examples
plot_taxon_table <- function(taxon_table, meta_table = NULL, ylabel = "stat"){
  # the input is a data.frame with rows as experiment, and columns as 6 taxon levles
  # the values are summarized, could be count (number of incidence), intensity, or other indexes, like types of alpha diversity

  if(is.null(meta_table)){
    taxon_table <- melt(as.matrix(taxon_table),varnames  = c("Experiment","Levels"), value.name = "value")

    p_combined <- ggplotly(ggplot(taxon_table, aes(x=Levels, y = value))+
                             geom_boxplot(alpha = 0.8, show.legend = FALSE, aes(fill=factor(Levels))) +
                             geom_jitter(position=position_jitter(width=0.3), alpha=0.9) +
                             scale_fill_brewer(palette="Dark2")+
                             #expand_limits(y = 0) +
                             ylab(ylabel)+ xlab("Level") + theme_bw()) %>%
      layout(autosize = F)

    p_grid <- ggplotly(ggplot(taxon_table, aes(x=Experiment, y = value))+
                         geom_boxplot(alpha = 0.8, show.legend = FALSE, aes(fill=factor(Levels))) +
                         geom_jitter(position=position_jitter(width=0.3), alpha=0.9) +
                         facet_grid(. ~ Levels )+
                         coord_flip()+
                         scale_fill_brewer(palette="Dark2")+
                         expand_limits(y = 0) +
                         ylab(ylabel)+ xlab("Experiment") + theme_bw()) %>%
      layout(autosize = F)

    plot_list <- list(combined_plot = p_combined, grid_plot =p_grid)

  }else{
    taxon_table<- cbind(taxon_table, meta_table) %>%
      reshape2::melt(id.vars = colnames(meta_table),variable.name = "Levels", value.name = "value")

    plot_list <- lapply(as.list(colnames(meta_table)[-1]),function(x){
      #x <- as.vector(x)
      ggplotly(ggplot(taxon_table, aes_string(x=x, y="value")) +
                 geom_boxplot(aes(fill = Levels))+
                 geom_jitter(position=position_jitter(width=0.3), alpha=0.9) +
                 scale_fill_brewer(palette="Dark2")+
                 coord_flip()+
                 ylab(ylabel)+ xlab("Level") + theme_bw()+
                 facet_grid(. ~ Levels ))
    })
    names(plot_list)  <- colnames(meta_table)[-1]

  }
  return(plot_list)
}

