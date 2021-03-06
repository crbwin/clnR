% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/investigating.r
\name{alpha.only}
\alias{alpha.only}
\title{Produces a singular estimate of Cronbach alpha using quosures}
\usage{
alpha.only(data, vector, neg.corr = FALSE)
}
\arguments{
\item{data}{data frame}

\item{vector}{quosure of items to analyze}

\item{neg.corr}{are some items negatively correlated? This reverses those items to get an accurate estimate of alpha}
}
\value{
individual Cronbach's alpha estimate
}
\description{
This function produces info about Cronbach's alpha for a group of variables
}
