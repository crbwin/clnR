% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting.r
\name{table.describe}
\alias{table.describe}
\title{Produces a formatted table of descriptives}
\usage{
table.describe(data, vars, names = NULL, copy = TRUE, alpha = FALSE)
}
\arguments{
\item{data}{data frame}

\item{vars}{quosure of items to analyze}

\item{names}{a vector of names to replace variable names for increased clarity}

\item{copy}{Would you like to copy the table to a spreadsheet or doc? The 'copiable' version is not formatted, such that it's easier to copy the matrix of information.}

\item{alpha}{Would you like to include Cronbach's alphas? This calls 'table.alpha' to calculate alphas for each composite}
}
\value{
a table of descriptives for a group of variables
}
\description{
This function produces a table of descriptive output for a list of variables.
}
