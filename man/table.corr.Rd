% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlations.r
\name{table.corr}
\alias{table.corr}
\title{Basic correlation table}
\usage{
table.corr(df, vars, rows = auto, cols = auto, copy = FALSE)
}
\arguments{
\item{df}{data frame}

\item{vars}{quosure of variables to analyze}

\item{rows}{number of variables used as rows; used in conjunction with cols. Default is all vars}

\item{cols}{number of variables used as columns; used in conjunction with rowss. Default is all vars}

\item{copy}{Should this table be 'copiable' or formatted?}
}
\description{
Basic correlation table
}
