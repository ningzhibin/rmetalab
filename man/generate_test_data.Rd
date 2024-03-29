% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_test_data.R
\name{generate_test_data}
\alias{generate_test_data}
\title{Generate a fake metaproteomic/proteomic data matrix, with grouping information}
\usage{
generate_test_data(seed = 1981)
}
\arguments{
\item{seed}{}
}
\value{
a list with a matrix, of 200 rows and 30 columns, and a meta table, with two columns, sample name and grouping
}
\description{
Generate a fake metaproteomic/proteomic data matrix, with grouping information
}
\examples{

test_data <- generate_test_data()
test_data$matrix
test_data$meta


}
