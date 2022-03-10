
#' tidy_MQ_summary to cleanup and orgnaize the summary.txt file
#'
#' @param df_summary.txt a data.frame directly read from
#'
#' @return a list with 3 values, summary_all, summary_rawfiles, set_experiment
#'
#' @export
#'
#' @examples
#'   file_path <- system.file("extdata","summary.txt", package = "metalab")
#'  summary_table <- read.delim(file_path, header = TRUE,check.names = FALSE, stringsAsFactors = FALSE) #
#'  t <- tidy_MQ_summary(summary_table)
#'  t$summary_all
#'  t$summary_rawfiles
#'  t$set_experiment
#'
#'
tidy_MQ_summary <- function(df_summary.txt){
  # the format of summary.txt mainly fall into three categories
  # df_summary.txt has to be a tidyverse tbl format, from read_tsv
  # remove the last line to organize into a data.frame

  last_line <- t(df_summary.txt[nrow(df_summary.txt),])
  last_line[last_line[,1] == ""] <- NA
  last_line[last_line[,1] == "0"] <- NA
  last_line <- last_line[-1,, drop = FALSE]
  last_line <- last_line[which(!is.na(last_line[,1])),, drop =  FALSE]
  colnames(last_line) <- ("values")

  summary <- df_summary.txt[-nrow(df_summary.txt),] # remove the last line

  # take out rows about raw files summary

  # if there are experiment design column,
  if(length(grep("Experiment", colnames(summary)))>0){

    # and if there are separate rows for experimental desgin,
    # otherwise, expereiment desgin is set with one raw file one experiment
    if(length(which(nchar(summary$Experiment) ==0)) >0){ # this
      df_rawfiles <-summary[which(nchar(summary$Experiment) >0),]

      # take out rows about experiment summary
      df_experiment <- summary[which(nchar(summary$Experiment) ==0),]

      return(list("summary_all" = last_line,
                  "summary_rawfiles" = df_rawfiles,
                  "summary_experiment" = df_experiment,
                  "set_experiment" = TRUE

      ))
    }else{

      df_rawfiles <-  summary
      return(list("summary_all" = last_line,
                  "summary_rawfiles" = df_rawfiles,
                  "set_experiment" = FALSE # even with experiment setup, still no need to do

      ))

    }

    # otherwise no need to do separate experiment display

  }else{
    df_rawfiles <-  summary
    return(list("summary_all" = last_line,
                "summary_rawfiles" = df_rawfiles,
                "set_experiment" = FALSE

    ))

  }

}
