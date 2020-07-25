

#' Ben Lira's function for assessing and dropping multivariate outliers. T
#'
#' The function provides information on multivariate outliers, but also produces
#' a dataset with multivariate outliers excluded, which can be saved to a new object, or used to overwrite the data set.
#' This calculates Mahalanobis distance for each observation and calculates multivariate outliers using Chi2 testing.
#'
#' @param data data frame
#' @param vars vector of subset of data to analyze. Takes either varnames or a numeric vector of column numbers.
#' @param exclude vector of subset of data to exclude from analysis. Takes either varnames or a numeric vector of column numbers.
#' @param p level at which to test the significance of outliers, by testing the Chi2 parameter of the alpha distribution.
#' @return cases excluded, a dataset of remaining cases, and a graph showing excluded vs. retained cases.
#' @export

bigO.drop <- function(data, vars = NULL, exclude = NULL, p = .001){
  require(tibble)

  alpha = 1 - p

  if(is.null(vars)){

    dat <- data %>% select(is.numeric)
  } else{

    dat <- data %>% select(all_of(vars)) %>% select(is.numeric)
  }

  if(!is.null(exclude)){

    dat <- data %>% select(-all_of(exclude)) %>% select(is.numeric)
  }


  mahal =  careless::mahad(dat)
  cutoff = qchisq(alpha, ncol(dat))
  remain = mahal < cutoff

  plot = ggplot(dat %>% mutate(ID = 1:nrow(dat), Distance = mahal, Outlier = remain), aes(ID, Distance, color = Outlier)) +
    geom_point(aes(alpha = .2)) +
    theme(legend.position= "none")

  print(plot)

  n = nrow(dat) - sum(remain, na.rm = TRUE)

  pct = paste0(round(n/nrow(dat)*100),"%")

  print(paste0(n, " (", pct, ")",  " cases were multivariate outliers"))

  print(enframe(remain) %>% filter(value==FALSE))

  return(data %>% filter(remain))
}
