
#' Tidy protein(proteinGroups.txt) or similiar prtoein table
#'
#' @param protein_table
#'
#' @return
#' @export
#'
#' @examples
tidy_protein_table <- function(protein_table){

  # extract the primary protein ID,
  protein.ids_split <- strsplit(as.vector(protein_table$"Protein.IDs"), ";| ") # this is a list of list of split names ; for maxquant result, space( ) for Kai's open-search result
  protein_primary_ids <- unlist(lapply(protein.ids_split, function(x) x[1])) # only keep the first one
  rownames(protein_table) <- protein_primary_ids # rename the rownames of the matrix

  # do the row wise filtering
  index_contaminant <- grep("\\+",protein_table[,grep( "contaminant", colnames(protein_table))]) # note that + is a special character
  index_reverse <- grep("\\+", protein_table$Reverse)
  index_to_remove <- c(index_contaminant,index_reverse)

  if(length(index_to_remove) >0){
    protein_table <- protein_table[-index_to_remove,] # filtered table
  }


  n_contaminant <- length(index_contaminant)
  n_reversed <- length(index_reverse)


  # extract the intensity column matrix
  if(any(grepl("LFQ.intensity.", colnames(protein_table)))){ # if there are LFQ intensity columns, take out the LFQ columns
    df_intensity <- protein_table[,grep("LFQ.intensity.", colnames(protein_table))]
    colnames(df_intensity)<-gsub("LFQ.intensity.", "", colnames(df_intensity))
  }else{ # otherwise take out intensity column， even only one column
    df_intensity <-   protein_table[,grep("Intensity.", colnames(protein_table)),drop =  FALSE]
    colnames(df_intensity)<-gsub("Intensity.", "", colnames(df_intensity))
  }

  # remove rows without any quantification value
  df_intensity <- exp(log(df_intensity))# this is going to remove the integer64 problem

  df_intensity <- df_intensity[-which(rowSums(df_intensity > 0) == 0),,drop = FALSE]
  df_intensity_Q100 <- df_intensity[which(rowSums(df_intensity > 0) == ncol(df_intensity)) , , drop = FALSE]
  df_intensity_Q50 <- df_intensity[which(rowSums(df_intensity > 0) >= ncol(df_intensity)*0.5) , , drop = FALSE]

  sparsity <- rowSums(df_intensity > 0) # here sparsity is number of present values
  df_sparsity <- as.data.frame(table(sparsity))
  df_sparsity <- df_sparsity[order(df_sparsity$sparsity,decreasing = TRUE),]
  df_sparsity$sparsity <- factor(df_sparsity$sparsity, levels = df_sparsity$sparsity)

  return(list("intensity_matrix" = df_intensity,
              "intensity_matrix_Q100" = df_intensity_Q100,
              "intensity_matrix_Q50" = df_intensity_Q50,
              "experiment" = colnames(df_intensity),
              "n_contaminant" = n_contaminant,
              "n_reversed" = n_reversed,
              "n_unique_peptides" = protein_table$Unique.peptides,
              "score" = protein_table$Score,
              "sparsity" = df_sparsity
              ))

}

# change log

# 20220222 fix the issue with converting integer64 data.frame to matrix, with simply do.call(cbind,df_intensity)
#             somehow rio::import will turn intensity columns into integer64 data type, but as.matrix will not be able to convert it correctly

#'  file_path <- system.file("extdata","proteinGroups.txt", package = "rmetalab")
#'  protein_table <- rio::import(file_path, header = TRUE,check.names = TRUE, stringsAsFactors = FALSE) #
#'  t <- tidy_protein_table(protein_table)
#'  t$intensity_matrix
#'  t$experiment
#'  str(t)
#'

