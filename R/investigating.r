#' Perform factor analysis and get useful output using quosures
#'
#' This function creates a composite variable of a group of individual items in your data frame.
#'
#' @param data data frame
#' @param vector quosure of items to analyze
#' @param factors the number of factors to fit
#' @param rotation Type of rotation; drawing from rotation options in the 'psych' package's factanal function.
#' @return factor analysis output and scree plot
#' @export

factorize <- function(data, vector, factors = 1, rotation = "varimax"){
  
  library(psych)
  fact <- data %>% select(!!!vector)
  
  corr_table <- round(cor(fact, use = "pairwise.complete.obs"), digits = 3) # you need to determine how NAs should be treated. SPSS uses deleting NAs pairwise as a default, so R should do the same in order to compare the results
  
  scree_plot <- psych::scree(corr_table, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE); scree_plot
  
  if(factors >= 2){
    fit <- factanal(na.omit(fact),
                    factors = factors, # how many factors to calculate
                    rotation = rotation)
    print(fit, digits=3, cutoff=.3, sort=TRUE)
  } else {
    factanal(na.omit(fact),
             factors = factors, # how many factors to calculate
             rotation = "none")
    #print(fit, digits=2, cutoff=.3, sort=TRUE)
  }
}


#' Produce Cronbach alpha output using quosures
#'
#' This function produces info about Cronbach's alpha for a group of variables
#'
#' @param data data frame
#' @param vector quosure of items to analyze
#' @param neg.corr are some items negatively correlated? This reverses those items to get an accurate estimate of alpha
#' @return cronbach alpha output from the psych package's 'alpha' function
#' @export


alphatize <- function(data, vector, neg.corr = FALSE){
  data %>%
    select(!!!vector) %>% # note that variables in vector must be pre-quoted using quos() function
    na.omit() %>%
    psych::alpha(check.keys = neg.corr)
}

#' Produces a singular estimate of Cronbach alpha using quosures
#'
#' This function produces info about Cronbach's alpha for a group of variables
#'
#' @param data data frame
#' @param vector quosure of items to analyze
#' @param neg.corr are some items negatively correlated? This reverses those items to get an accurate estimate of alpha
#' @return individual Cronbach's alpha estimate
#' @export

alpha.only <- function(data, vector, neg.corr = FALSE){
  
  subdat <- data %>% select(!!!vector) %>% na.omit()
  tt <- tryCatch(psych::alpha(subdat), error=function(e) e, warning=function(w) w)
  
  if(is(tt, "warning")){
    temp <-
      data %>%
      select(!!!vector) %>% # note that variables in vector must be pre-quoted using quos() function
      na.omit() %>%
      psych::alpha(check.keys = TRUE)
    
  }else{
    
    temp <-
      data %>%
      select(!!!vector) %>% # note that variables in vector must be pre-quoted using quos() function
      na.omit() %>%
      psych::alpha(check.keys = neg.corr)
    
  }
  
  temp$total$raw_alpha %>% broman::myround(2)
  # Example:
  # var_list <- quos(disp, wt, cyl)
  # new_alpha <- alphatize(mtcars, var_list)
}

#' Produce a table of frequencies for a group of variables using quosures
#'
#' This function produces a table of frequencies for each of variables in a group
#'
#' @param data data frame
#' @param vars quosure of items to analyze
#' @return a data.frame of frequencies for analyzed variables
#' @export

table.freq <- function(data, vars){
  
  data %>% select(!!!vars) %>%
    gather() %>%
    group_by(key,value) %>%
    mutate(n =1) %>%
    summarise(f = sum(n)) %>%
    ungroup() %>%
    spread(value,f) #%>% mutate_at(c(1:10), ~replace(., is.na(.), " ")) %>% kable() %>% kable_styling()
  
}

