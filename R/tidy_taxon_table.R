#' tidy taxon table to facilitate downstream analysis
#'
#' @param data_table
#'
#' @return list of values/tables/list
#' @export
#'
#' @examples
#'
#'
tidy_taxon_table <- function(data_table){

  colnames(data_table)<-gsub("LFQ.intensity.", "", colnames(data_table))
  colnames(data_table)<-gsub("Intensity.", "", colnames(data_table))
  experiment_number <- ncol(data_table)-10
  experiment <- colnames(data_table)[11:ncol(data_table)]

  data_table_binary <- data_table
  data_table_binary[data_table_binary>0] = 1


  extract_species <- data_table[data_table$Rank == 'Species', -c(2:10)]  %>%  remove_rownames %>% column_to_rownames(var="Name")
  extract_genus <- data_table[data_table$Rank == 'Genus', -c(2:10)]  %>%  remove_rownames %>% column_to_rownames(var="Name")
  extract_family <- data_table[data_table$Rank == 'Family', -c(2:10)]  %>%  remove_rownames %>% column_to_rownames(var="Name")
  extract_order <- data_table[data_table$Rank == 'Order', -c(2:10)]  %>%  remove_rownames %>% column_to_rownames(var="Name")
  extract_class <- data_table[data_table$Rank == 'Class', -c(2:10)]  %>%  remove_rownames %>% column_to_rownames(var="Name")
  extract_phylum <- data_table[data_table$Rank == 'Phylum', -c(2:10)]  %>%  remove_rownames %>% column_to_rownames(var="Name")


  taxon_summary_widetable_list <- list("species" = extract_species,
                                       "genus" = extract_genus,
                                       "family" = extract_family,
                                       "order" = extract_order,
                                       "class" = extract_class,
                                       "phylum" = extract_phylum)

  # get the species level presence or not data
  ID_number_table <- lapply(taxon_summary_widetable_list, function(x) vegan::specnumber(x, MARGIN = 2)) %>% as.data.frame

  index_diversity_shannon <- lapply(taxon_summary_widetable_list, function(x) vegan::diversity(x,index = "shannon", MARGIN = 2)) %>% as.data.frame
  index_diversity_simpson <- lapply(taxon_summary_widetable_list, function(x) vegan::diversity(x,index = "simpson", MARGIN = 2)) %>% as.data.frame
  index_diversity_invsimpson <- lapply(taxon_summary_widetable_list, function(x) vegan::diversity(x,index = "invsimpson", MARGIN = 2)) %>% as.data.frame

  # shannon <- vegan::diversity(extract_species_binary,index = "shannon", MARGIN = 2)
  # simpson <- vegan::diversity(extract_species_binary,index = "simpson", MARGIN = 2)
  # invsimpson <- vegan::diversity(extract_species_binary,index = "invsimpson", MARGIN = 2)

  index_diversity_fisher_species <- vegan::fisher.alpha(extract_species, MARGIN = 2)

  alpha_diversity <- list(index_diversity_shannon = index_diversity_shannon,
                          index_diversity_simpson = index_diversity_simpson,
                          index_diversity_invsimpson = index_diversity_invsimpson,
                          index_diversity_fisher_species =index_diversity_fisher_species)


  taxon_summary_longtable_list = list( "species" = melt(as.matrix(extract_species),varnames = c("Name", "Sample"),value.name = "Intensity"),
                      "genus" = melt(as.matrix(extract_genus),varnames = c("Name", "Sample"),value.name = "Intensity"),
                      "family" = melt(as.matrix(extract_family),varnames = c("Name", "Sample"),value.name = "Intensity"),
                      "order" = melt(as.matrix(extract_order),varnames = c("Name", "Sample"),value.name = "Intensity"),
                      "class" = melt(as.matrix(extract_class),varnames = c("Name", "Sample"),value.name = "Intensity"),
                      "phylum" = melt(as.matrix(extract_phylum),varnames = c("Name", "Sample"),value.name = "Intensity")
  )

  return(list(taxon_summary_widetable_list = taxon_summary_widetable_list,
              taxon_summary_longtable_list = taxon_summary_longtable_list,
              ID_number_table =ID_number_table,
              alpha_diversity = alpha_diversity,
              experiment_number = experiment_number,
              experiment = experiment

  ))
}

