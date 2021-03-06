% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/investigating.r
\name{alphatize}
\alias{alphatize}
\title{Produce Cronbach alpha output using quosures}
\usage{
alphatize(data, items, names = NULL, neg.corr = FALSE)
}
\arguments{
\item{data}{data frame}

\item{items}{quosure of items to analyze}

\item{names}{optional list item names or wording to append to the table for increased interpretability}

\item{neg.corr}{are some items negatively correlated? This reverses those items to get an accurate estimate of alpha}
}
\value{
cronbach alpha output from the psych package's 'alpha' function
}
\description{
Using the psych package's alpha, his function produces info about Cronbach's alpha for a group of variables.
The output mirrors the output from STATA's alpha command, which
is more succinct and understandable than the psych package's alpha function default output.
}
