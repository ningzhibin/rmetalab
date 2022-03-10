
#' Generate a COG category on the fly
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' COG_categry()
COG_categry <- function(){
  df<- data.frame(categry = c("D", "M","N","O","T","U","V","W","Y","Z","A","B","J","K","L","C","E","F","G","H","I","P","Q","R","S"),
                  name = c("Cell cycle control, cell division, chromosome partitioning",
                            "Cell wall/membrane/envelope biogenesis",
                            "Cell motility",
                            "Post-translational modification, protein turnover, and chaperones",
                            "Signal transduction mechanisms",
                            "Intracellular trafficking, secretion, and vesicular transport",
                            "Defense mechanisms",
                            "Extracellular structures",
                            "Nuclear structure",
                            "Cytoskeleton",
                            "RNA processing and modification",
                            "Chromatin structure and dynamics",
                            "Translation, ribosomal structure and biogenesis",
                            "Transcription",
                            "Replication, recombination and repair",
                            "Energy production and conversion",
                            "Amino acid transport and metabolism",
                            "Nucleotide transport and metabolism",
                            "Carbohydrate transport and metabolism",
                            "Coenzyme transport and metabolism",
                            "Lipid transport and metabolism",
                            "Inorganic ion transport and metabolism",
                            "Secondary metabolites biosynthesis, transport, and catabolism",
                            "General function prediction only",
                            "Function unknown"),

                  class = c(rep("CELLULAR PROCESSES AND SIGNALING",10),
                            rep("INFORMATION STORAGE AND PROCESSING",5),
                            rep("METABOLISM",8),
                            rep("POORLY CHARACTERIZED",2)),
                  color = generate_colors(25)
  )
  return(df)
}

#
