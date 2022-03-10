#' find 3 chains combo for a lipid structure
#'
#' find_lipid_combo start with more than 3 observed fatty acid chains with defined carbo and double bound number to find the
#' combo to achieve a target lipid structure. The three chains could be the same (replacement sampling).
#'
#' @param combo_target a string, general format as carbon number:double bound number, like 51:4
#' @param chains_observed a string vector, consists of all observed fatty acid chains, with  general format as carbon number:double bound number,
#' @return a data.frame, with all possible 3 chain compositions. The order of the chain does not matter.
#'
#' @seealso \code{\link{combn}}
#'
#' @examples
#'
#' combo_target <- "51:4"
#' chains_observed <- c("18:1","18:2","18:3", "17:1", "17:2" ,"16:0" ,"16:1", "15:0" ,"15:1")
#'
#' r <- find_lipid_combo(combo_target,chains_observed)
#'
#' #error run
#' r <- find_lipid_combo(combo_target,"dat")
#' r <- find_lipid_combo(combo_target,c("18:1", "bbb", "17:1", "17:2") ) #notworking, can run, but result is not right



#' @export
#'

find_lipid_combo <- function(combo_target,chains_observed){

  # some parameter check
  if(length(chains_observed) < 3){
    result <- data.frame("There are less than 3 chains inputed, not able to calcuate")
    colnames(result) <- "Error Message!"
    #stop("There are less than 3 chains inputed, not able to calcuate")
    result
  }else if(!all(str_detect(chains_observed, "\\d:\\d"))){# check if the pattern is right, digital, then :, then ditigal
    result <- data.frame(paste0("The ", which(!str_detect(chains_observed, "\\d:\\d")), " entry is not in the right format"))
    colnames(result) <- "Error Message!"
    #stop(paste0("The ", which(!str_detect(chains_observed, "\\d:\\d")), " entry is not in the right format"))
    result
  } else {
    for(i in 1: length(chains_observed)){
      chains_observed[i]
      grep(":",chains_observed[i])
    }


    All_combinations <- expand.grid(rep(list(chains_observed), 3))

    All_combinations_sorted <- t(as.data.frame(apply(All_combinations, 1, sort)))

    All_combinations_sorted_unique <- All_combinations_sorted[!duplicated(All_combinations_sorted), ]



    result  <- as.data.frame(matrix(nrow = 1, ncol = 3))
    colnames(result) <- paste0("chain",1:3)
    j <- 1
    for(i in 1: nrow(All_combinations_sorted_unique)){
      ll <- All_combinations_sorted_unique[i,]
      sub_combine <- rowSums(as.data.frame(lapply(ll, function(x){as.numeric(unlist(strsplit(x, ":")))})))
      pattern <- paste0(sub_combine[1],":",sub_combine[2])
      if(pattern == combo_target){
        result[j,] <- ll
        j <- j+1
      }
    }

    if(is.na(result[1,1])){

      result <- data.frame("No combination find!")
      colnames(result) <- "Error Message!"
      result
    }else{

      result
    }

  }

}






