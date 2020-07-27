

#' Produce a formatted table of frequencies for a single variable
#'
#'
#' @param data data frame
#' @param wide should the table be full width or compressed?
#' @return a formatted table of proportions of frequency for a single variable
#' @export

tabshell <- function(data, ..., wide = TRUE){

  vector <- quos(...)

  tab <- data %>%
    filter(!is.na(!!!vector)) %>%
    group_by(!!!vector) %>%
    summarise(n = n()) %>%
    mutate('Proportion' = paste0(round((n / sum(n)), 3)*100, "%"))

  tab[, -c(2)]%>% t() %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = wide)
}

#' Produce a short report of descriptives
#'
#' This function produces descriptives in a 'nutshell'
#'
#' @param data data frame
#' @param vector quosure of items to analyze
#' @param type is this for a single variable ('lonely') or a group of variables ('family')?
#' @return a table of n, mean, and sd for variables
#' @export

nutshell <- function(data, vector, type = "family"){

  if(type=="lonely"){

    vector <- enquo(vector)

    desc <- data %>%
      select(!!vector) %>%
      psych::describe %>% round(., digits = 2) %>% data.frame()

  } else{

    desc <- data %>%
      select(!!!vector) %>%
      describe %>% round(., digits = 2) %>%  data.frame()

  }

  scale <- paste(desc$min, "-", desc$max)

  desc %>%
    select(c("n", "mean", "sd")) %>%
    cbind(., scale) %>%
    kable %>% kable_styling(bootstrap_options = c("striped", "hover"))
}






#' Produce Cronbach alpha if dropped output using quosures
#'
#' This function reports the Cronbach's alpha for a group of items if each item were dropped.
#'
#' @param data data frame
#' @param items quosure of items to analyze
#' @param name optional vector of item names/wording
#' @param neg.corr are some items negatively correlated? This reverses those items to get an accurate estimate of alpha
#' @return cronbach alpha output from the psych package's 'alpha' function
#' @export

alpha.drop <- function(data, items, name=NULL, neg.corr = FALSE){

  alpha <- data %>% select(!!!items) %>% psych::alpha( check.keys = neg.corr)

  m_names <- rownames(alpha$alpha.drop)

  m_names <- c(m_names, "", "Overall")

  a_drop <- round(alpha$alpha.drop[, 2], digits = 3)
  row.drop <- which(a_drop>alpha$total$std.alpha) %>% as.numeric()
  a_drop <- c(a_drop, "", round(alpha$total$std.alpha, digits = 3))
  n <- length(a_drop)

  if (is.null(name)){


    tab <- cbind("Item" = m_names, "Alpha if dropped" = a_drop)

    if(!is_empty(row.drop)){

      tab %>% kable(align=c('l', 'c')) %>% row_spec( n, bold = T) %>% row_spec(row.drop, italic = T) %>% kable_styling( full_width = F)
    }else{

      tab %>% kable(align=c('l', 'c')) %>% row_spec(n, bold = T, italic = T) %>% row_spec(n, italic = T) %>% kable_styling( full_width = F)
    }

  } else {

    blank <- c(name, "", "")

    tab <- cbind(" " = blank, "Item" = m_names, "Alpha if dropped" = a_drop)

    if(!is_empty(row.drop)){

      tab %>% kable(align=c('l','l', 'c')) %>% row_spec( n, bold = T) %>% row_spec(row.drop, italic = T) %>% kable_styling( full_width = F)
    }else{

      tab %>% kable(align=c('l','l', 'c')) %>% row_spec(n, bold = T, italic = T) %>% row_spec(n, italic = T) %>% kable_styling( full_width = F)
    }
  }



}

#' Produce Cronbach alpha output using quosures
#'
#' This function produces a vector of Cronbach's alphas for a list of composite variables.
#' Note that this function requires quosures including the consituent items of each composite
#' to exist and have already been created. These are necessary to assess alpha for each composite.
#'
#' @param data data frame
#' @param x quosure of composites to analyze
#' @param neg.corr are some items negatively correlated? This reverses those items to get an accurate estimate of alpha
#' @return a vector of Cronbach's alphas for a list of composite variables
#' @export

table.alpha <- function(data, x){

  var.name <- c()
  qq <- c()
  alphas <- c()

  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }


  for(i in 1:length(x)){

    var.name <- data %>% select(!!!x[[i]]) %>% names()


    if(substrRight(var.name,2)=="_r"){

      var.name <- substr(var.name, 1, nchar(var.name) - 2)
    } else {

    }


    gg <- paste0("q_", var.name)

    gg2 <- as.name(gg)

    qq <- enquo(gg2)

    if(!exists(gg)){

      alphas[i] <- " "

    } else if(length(eval(rlang::quo_get_expr(qq)))>2){

      alphas[i] <- data %>% alpha.only(eval(rlang::quo_get_expr(qq)))
    } else{

      alphas[i] <- " "
    }

  }

  if(sum(nchar(alphas)) < length(x)+1){

    print("Warning: Alphas for composites have not been computed. To compute, quosures including the constituent variables of each composite must exist in the form of 'q_comp_name'.")
  } else {


  }

  alphas
}

#' Produces a formatted table of descriptives
#'
#' This function produces a table of descriptive output for a list of variables.
#'
#' @param data data frame
#' @param vars quosure of items to analyze
#' @param names a vector of names to replace variable names for increased clarity
#' @param copy Would you like to copy the table to a spreadsheet or doc? The 'copiable' version is not formatted, such that it's easier to copy the matrix of information.
#' @param alpha Would you like to include Cronbach's alphas? This calls 'table.alpha' to calculate alphas for each composite
#' @return a table of descriptives for a group of variables
#' @export

table.describe <- function(data, vars, names = NULL, copy = TRUE, alpha = FALSE){


  if(!is.null(names)){


    gr.desc <- data %>% select(!!!vars) %>% psych::describe() %>% data.frame() %>% select(-c(1, 6, 7, 10:13))
    gr.desc <- gr.desc %>% cbind.data.frame(names, .)

  } else{

    gr.desc <- data %>% select(!!!vars) %>% psych::describe() %>% data.frame() %>% select(-c(1, 6, 7, 10:13)) %>% tibble::rownames_to_column("names")


  }


  round.new <- function(x){

    round(x, 2)
  }

  if(alpha==TRUE){

    alpha <- table.alpha(data, vars)

    if(copy==TRUE){


      gr.desc %>% mutate(across(where(is.numeric), round.new)) %>% cbind.data.frame(., alpha) %>% relocate(., "alpha", .before = "mean") %>% kable() %>%
        kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 12, full_width = F)

    } else{

      gr.desc %>% mutate(across(where(is.numeric), round.new)) %>% cbind.data.frame(., alpha) %>% relocate(., "alpha", .before = "mean")
    }


  }else{

    if(copy==TRUE){
      gr.desc %>% mutate(across(where(is.numeric), round.new)) %>% kable() %>% kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 12, full_width = F)

    } else{

      gr.desc %>% mutate(across(where(is.numeric), round.new))
    }

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

partR <- function(model){

  sqrt(rsq::rsq.partial(model)$partial.rsq[1]) %>% round(., digits = 3)

}
