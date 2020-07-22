#' Generate a composite variable
#'
#' This function creates a composite variable of a group of individual items in your data frame.
#'
#' @param data data frame
#' @param new name of new variable to create
#' @param ingred.list quosure of variables to average into composite
#' @return a new variable composite of individual items
#' @export

cv_bake <- function(data, new, ingred.list){
  new <- enquo(new)
  data %>%
    rowwise() %>%
    mutate(!!quo_name(new) := mean(c(!!!ingred.list), na.rm = TRUE)) %>%
    ungroup()
}