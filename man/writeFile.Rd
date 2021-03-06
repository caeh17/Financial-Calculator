% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IRR_calculator.R
\name{writeFile}
\alias{writeFile}
\title{Writes out the results of the calculations.}
\usage{
writeFile(results, fileName)
}
\arguments{
\item{results}{the data frame containing the results of the calculations.}

\item{fileName}{the name of the original file analysed.}
}
\description{
This method creates a new .csv file with '-analysis-' and a timestamp appended
to the original name. If succesful, a message about the name of the new file
is printed to the console.
}
