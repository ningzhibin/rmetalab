
#' tidy the peptide table input into a list.
#'
#' Cleaning include removing contaminant, reverse hits, extract the peptide intensity matrix for plotting,
#' extract the experiment design, put the peptide length, sequence, missing cleavage site into vectors ready to use
#'
#'
#' @param peptide_table a data.frame read from peptide.txt from maxquant or peptide table with required format(columns: Sequence,Score,Charges,Length, Intensity or LFQ.Intensity columns). check.names = TRUE options to remove the space in column names
#'
#' @return an object list, including a peptide intensity matrix, vectors of peptide_sequence,length, misscleavage, charges, etc.
#'
#' @examples
#'  file_path <- system.file("extdata","peptides_1000.txt", package = "rmetalab")
#'  peptide_table <- rio::import(file_path, header = TRUE,check.names = TRUE, stringsAsFactors = FALSE) #
#'  t <- tidy_peptide_table(peptide_table)
#'  t$intensity_matrix
#'  t$experiment
#'  str(t)
#'
#' @export
#'
tidy_peptide_table <- function(peptide_table){

  #peptide_sequence <- peptide_table$Sequence # only keep the first one

  # do the row wise filtering, the following applies to maxquant outputs
  index_contaminant <- grep("\\+", peptide_table$Potential.contaminant) # note that + is a special character
  index_reverse <- grep("\\+", peptide_table$Reverse)

  index_to_remove <- c(index_contaminant,index_reverse)

  if(length(index_to_remove) > 0){ # some times there are no rows to remove
    peptide_table <- peptide_table[-index_to_remove,] # filtered table
    #peptide_sequence <- peptide_sequence[-index_to_remove] # filtered ids
  }


  n_contaminant <- length(index_contaminant)
  n_reversed <- length(index_reverse)
  peptide_sequence <- peptide_table$Sequence
  rownames(peptide_table) <-peptide_sequence



  # extract the intensity column matrix
  if(any(grepl("LFQ.intensity.", colnames(peptide_table)))){ # if there are LFQ intensity columns, use the LFQ columns for the intensity

    # todo, TMT file
    df_intensity <- peptide_table[,grep("LFQ.intensity.", colnames(peptide_table), ignore.case = TRUE)]
    colnames(df_intensity) <- gsub("LFQ.intensity.", "", colnames(df_intensity), ignore.case = TRUE) # rename the column

  }else{ # otherwise take out intensity column

    df_intensity <-   peptide_table[,grep("Intensity.", colnames(peptide_table), ignore.case = TRUE),drop =  FALSE]
    colnames(df_intensity)<-gsub("Intensity.", "", colnames(df_intensity), ignore.case = TRUE)

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


  return(list("intensity_matrix" = df_intensity,# filtered, with at least one value
              "intensity_matrix_Q100" = df_intensity_Q100,
              "intensity_matrix_Q50" = df_intensity_Q50,
              "experiment" = colnames(df_intensity),
              "peptide_sequence" =peptide_sequence,
              "n_contaminant" = n_contaminant,
              "n_reversed" = n_reversed,
              "score" = peptide_table$Score,
              "Charges" =peptide_table$Charges,
              "length" = peptide_table$Length,
              "misscleavage" = peptide_table$"Missed.cleavages",
              "sparsity" = df_sparsity

  ))
}
# change log
# 20220222 fix the issue with converting integer64 data.frame to matrix, with simply do.call(cbind,df_intensity)
#             somehow rio::import will turn intensity columns into integer64 data type, but as.matrix will not be able to convert it correctly
# 20220218 add more functions to do Q 100 filtering
# 0.2 change column name support to name.check = TRUE to replaced while space with coma
