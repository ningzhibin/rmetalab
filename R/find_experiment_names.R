#' Extract experiment names from Metalab result table
#'
#' In Metablab, the quantity information (intensity) are organized in result tables, with column headers usually starting with names like LFQ intensity (from LFQ quantification), Intensity (from raw data, labeled data, including TMT) etc. This function can automatically extract the experiment names for easy plotting or checking if table are consistent with meta_table provided.
#'
#' @param data_table could be peptide, protein, taxon and function table
#'
#' @return a list, with both index and names, and the data.table with new experiment names
#' @export
#'
#' @examples
#'
find_experiment_names <- function(data_table){

  col_names <-colnames(data_table)

  if(any(grepl("LFQ.intensity.", col_names))){ # if there are LFQ intensity columns, use the LFQ columns for the intensity

    experiment_index <- grep("LFQ.intensity.", col_names, ignore.case = TRUE)
    experiment_names <- col_names[experiment_index]
    experiment_names <- gsub("LFQ.intensity.", "", experiment_names, ignore.case = TRUE)
    colnames(data_table) <- gsub("LFQ.intensity.", "", col_names, ignore.case = TRUE) # rename the column
  }else{ # otherwise take out intensity column

    experiment_index <- grep("Intensity.", col_names, ignore.case = TRUE)
    experiment_names <- col_names[experiment_index]
    experiment_names <- gsub("Intensity.", "", experiment_names, ignore.case = TRUE)
    colnames(data_table) <- gsub("Intensity.", "", col_names, ignore.case = TRUE) # rename the column

  }
  return( list(experiment_names = experiment_names,
               experiment_index = experiment_index,
               data_table = data_table
  )
  )
}


