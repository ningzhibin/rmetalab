#' instal packages automatically from CRAN or bioconductor
#'
#' This function will automatically install and library/load the package when invoked.
#' It is a quick wrapper of of the install.package and biocoductor bioLite.
#' It also does not need quote on the package name.
#'
#' This function is just for quick and easy install and library usage. It does not consider installation from github yet.
#' If the target pacakge has already been installed, no installation will be proceeded.
#' With R version 3.5 or greater, Bioconductor packages installing start using BiocManager. Therefore this comand only woorks for biocondcutior for R 3.5 or greater
#'
#' @param x package name, with or without the quote both works
#'
#' @return None The same as intall.packages
#
#' @seealso \code{\link{install.packages}}
#'
#' @examples
#' install.packages.auto(roxygen2) # from CRAN
#' install.packages.auto(qvalue) # from bioconductor

#' @export

install.packages.auto <- function(x) {
  # setting up the miror
  local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cloud.r-project.org/"
    options(repos = r)
  })


  x <- as.character(substitute(x))

  if(isTRUE(x %in% .packages(all.available=TRUE))) {
    eval(parse(text = sprintf("require(\"%s\")", x)))
  } else {
    update.packages(ask= FALSE) #update installed packages.
    eval(parse(text = sprintf("install.packages(\"%s\", dependencies = TRUE)", x)))
  }
  if(isTRUE(x %in% .packages(all.available=TRUE))) {
    eval(parse(text = sprintf("require(\"%s\")", x)))
  } else {
    #source("http://bioconductor.org/biocLite.R")
    #With R version 3.5 or greater, install Bioconductor packages using BiocManager;
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")

    #biocLite(character(), ask=FALSE) #update installed packages.
    eval(parse(text = sprintf("BiocManager::install(\"%s\")", x)))
    eval(parse(text = sprintf("require(\"%s\")", x)))
  }


  # for github
  # if(isTRUE("metalab" %in% .packages(all.available=TRUE))) {
  #   eval(parse(text = sprintf("require(\"%s\")", "metalab")))
  # } else {
  #   eval(parse(text = sprintf("install_github(\"%s\", dependencies = TRUE)", "ningzhibin/metalab")))
  # }


}
