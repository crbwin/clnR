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
#' Using the psych package's alpha, his function produces info about Cronbach's alpha for a group of variables.
#' The output mirrors the output from STATA's alpha command, which
#' is more succinct and understandable than the psych package's alpha function default output.
#'
#' @param data data frame
#' @param items quosure of items to analyze
#' @param names optional list item names or wording to append to the table for increased interpretability
#' @param neg.corr are some items negatively correlated? This reverses those items to get an accurate estimate of alpha
#' @return cronbach alpha output from the psych package's 'alpha' function
#' @export


alphatize <- function (data, items, names = NULL, neg.corr = FALSE)
{
  # create object containing output from the psych package's alpha function
  alpha <- data %>% select(!!!items) %>% psych::alpha(check.keys = neg.corr)

  #create a vector that will make up the first column of output
  m_names <- rownames(alpha$alpha.drop)
  m_names <- c(m_names, "", "Overall")

  #create vector that will make up second column of output
  a_drop <- round(alpha$alpha.drop[, 2], digits = 3)
  row.drop <- which(a_drop > alpha$total$std.alpha) %>% as.numeric()
  a_drop <- c(a_drop, "", round(alpha$total$std.alpha,
                                digits = 3))

  n <- length(a_drop)

  #creating other columns in STATA output: obs, sign, item-test correlation, item-rest correlation, average interitem cov, alpha if dropped.
  obs <- alpha$item.stats$n
  obs <- c(obs, "", "")

  itc <- alpha$item.stats$r.cor
  itc<- c(round(itc, 3), " ", " ")
  sign <- ifelse(itc>0 & itc<1, "+",
                 ifelse(itc==" ", "",
                        ifelse(itc<0, "-", "")))
  irc <- alpha$item.stats$r.drop
  irc <- c(round(irc, 3), "", "")
  aic <- alpha$alpha.drop$average_r
  aic <- c(round(aic, 3), "", round(alpha$total$average_r, 3))

  if (is.null(names)) {

    tab <- cbind.data.frame(Item = m_names, obs, sign, `item-test correlation` = itc, `item-rest correlation` = irc, `avg inter-item correlation` = aic, `Alpha if dropped` = a_drop)

    if (!is_empty(row.drop)) {

      tab %>% kable(align = c("l", "c")) %>%
        row_spec(n, bold = T) %>% row_spec(row.drop,
                                           italic = T) %>% kable_styling(full_width = T)
    }
    else {

      tab %>% kable(align = c("l", "c")) %>%
        row_spec(n, bold = T, italic = T) %>% row_spec(n,
                                                       italic = T) %>% kable_styling(full_width = T)
    }
  }
  else {

    blank <- c(names, "", "")
    tab <- cbind.data.frame(` ` = blank, Item = m_names, obs, sign, `item-test correlation` = itc, `item-rest correlation` = irc, `average inter-item correlation` = aic, `Alpha if dropped` = a_drop)

    if (!is_empty(row.drop)) {

      tab %>% kable(align = c("l", "l", "c")) %>%
        row_spec(n, bold = T) %>% row_spec(row.drop,
                                           italic = T) %>% kable_styling(full_width = T)
    }
    else {

      tab %>% kable(align = c("l", "l", "c")) %>%
        row_spec(n, bold = T, italic = T) %>% row_spec(n,
                                                       italic = T) %>% kable_styling(full_width = T)
    }
  }
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

