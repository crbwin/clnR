
#############Formatted correlation table

# Function 1

### function to create descriptive output for APA tables

new.describe <- function(df, vars){


  descr.output <- df %>% dplyr::select(!!!vars) %>%
    psych::describe() %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    dplyr::select(var,mean,sd,n, min, max)

  descr.output %<>% mutate(


    mean.chr = ifelse(min==0 & max==1, paste0(round(mean*100, 2), "%"),
                      ifelse(mean > 999, formatC(mean, format="d", big.mark=","),
                             round(mean, 2))),
    sd.chr = ifelse(min==0 & max==1, " ",
                    ifelse(mean > 999, formatC(sd, format="d", big.mark=","), round(sd, 2))),
    n.chr = formatC(n, format="d", big.mark=",")

  )


  descr.output2 <- descr.output %>% dplyr::select(var, "M" = mean.chr, "SD" = sd.chr, "n" = n.chr) %>% t() %>% data.frame() %>% tibble::rownames_to_column("var")

  header.true <- function(df) {

    new.col <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)

    names(df) <- new.col[1, ]
    df[-1,]
  }


  descr.output2 <- header.true(descr.output2)

  descr.output2 <- data.frame(lapply(descr.output2, as.character), stringsAsFactors=FALSE)

  descr.output2

}


# FUNCTION 2

### Function to compute correlation matrix and format stars

corstars <- function(x){
  require(Hmisc)

  col.end <- ncol(x)
  col.start <- 1
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P

  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***",
                    ifelse(p < .01, "**",
                           ifelse(p < .05, "*", # significant
                                  ifelse(p < 0.1, "†", " ")))) #marginal

  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]


  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)


  return(Rnew)
}


# FUNCTION 3

### New function to wrap `corstars()` and output a correlation table formatted for APA

#' Generate a formatted, APA-ready correlation table with descriptives
#'
#' Below is code for producing formatted, paper-ready correlation tables with 'HARcorr' (i.e. hot-and-ready correlations; I like fun function names, sue me).
#' I tried to write the code to minimize the amount of packages the function required, and to use packages that you probably already use frequently.
#' The code produces bivariate correlation tables complete with many features that align the output with APA style and with standards and information you might
#' want for publishing your research. The main features include:

#' Bivariate correlations displayed on the lower half of the correlation matrix (omitted for the upper half), with 1's on the diagonal omitted.
#' Stars to indicate significance level, with a dagger indicating marginal significance (p < .10)
#' leading 0's removed from correlations; correlations rounded to 2 decimal places
#' Numbered variable names vertically, along with corresponding numbers horizontally.
#' Descriptive statistics for each variable (mean, standard deviation, n) displayed for each variable at the bottom of the matrix.
#' Per APA, numbers greater than 1,000 have commas (1,000 vs. 1000), and means and standard deviations between [-1, 1] that have a maximum/minimum
#' of greater than 1/-1 have a leading 0 (e.g. 0.57). "Means" of binary variables are presented as percentages and SD for these variables are omitted.
#' The options to include headers for groups of variables (e.g. including a header "Covariates" before rows of covariate variables)

#' For me, the inclusion of APA formatting, numbered variables, and descriptives are a huge improvement over previous correlation table functions I've used,
#' and save me a lot of time in formatting tables in papers.
#'
#' @param df data frame
#' @param vars quosure of variables to analyze
#' @param describe allows you to toggle on/off the descriptives (mean, standard deviation, and n) at the bottom of the table by specifying describe = TRUE/FALSE.
#' @param numbers allows you to toggle on/off the numbers used to reference variables on the horizontal axis by specifying numbers = TRUE/FALSE. If numbers = FALSE, variable names are used as the horizontal axis reference.
#' @param headers The options headers and spots are used together. They each take a vector as an argument. Headers specifies subheaders for groupings of variables; headers takes a character vector.
#' @param spots spots specifies the row before which you would like to insert the headers. Spots takes a numeric vector.
#' @param copy Copy allows you to toggle on/off the table formatting. If copy = FALSE, kable() formatting is applied to make the table formatted nicely for easy of use and presentation. If copy = TRUE, the formatting is removed and HARcorr() returns a data table, which makes the values more easily copied into other programs such as Excel or Word.
#' @param names a character vector containing full names of variables. These replace variable names to increase clarity.
#' @param full.labels Would you also like names on the top axis?
#' @return a formatted correlation table
#' @export

HARcorr <- function(df, vars, describe = TRUE, numbers = TRUE, headers = NULL, spots = NULL, copy = FALSE, names = NULL, full.labels = FALSE, partial = FALSE, control.vars = NULL, triangle = "lower"){

  ### General correlation function
  #### Change number or names of rows and columns to output only certain rows/columns.


  if(partial==FALSE){

  if(is.null(names)){

    corrtab <- df %>%
      dplyr::select(!!!vars) %>%
      corstars() %>%
      rownames_to_column(., var = "var") %>%
      # mutate(var = new_row_names(.$var)) %>%
      slice(-1)
  } else{

    corrtab <- df %>%
      dplyr::select(!!!vars) %>%
      corstars() %>%
      cbind.data.frame(var = names, .) %>%
      # mutate(var = new_row_names(.$var)) %>%
      slice(-1)

  }

  }else{


    auto <- 0

    corrtab <- df %>%
      corstarsl.all.kiIN3(., vars, control.vars, tri = triangle, round = round) %>%
      tibble::rownames_to_column(., var = "var")
    # mutate(var = new_row_names(.$var)) %>%
    # slice(-1)


  if(!is.null(names)){

    if(triangle=="lower"){
      corrtab <- corrtab %<>%
        mutate(var = names) #%>%
      # mutate(var = new_row_names(.$var)) %>%
      #slice(-1)
      colnames(corrtab) <- c("var", names[1:length(names)-1])

    } else if(triangle=="upper"){
      colnames(corrtab) <- c("var", names)

      # corrtab %<>% mutate(
      #
      #   var = names[1:length(names)-1],
      #   .before = .[, 1]
      # )
      corrtab[,1] <- names[1:length(names)-1]

    } else {

      print("Please choose 'triangle = upper' or 'triangle = lower'.")
    }


  } else{

  }

  }

  ### APA formatting: Adding in numbered y labels  (does not compute if )

  corrtab %<>% tibble::add_row(var = names(corrtab)[2], .before = 1)
  corrtab %<>% dplyr::mutate(
    ID = row_number(),
    dot = ". ",
    num.var = paste0(ID, dot, var)
  )

  corrtab %<>% dplyr::select(-c(ID, dot, var))
  corrtab %<>% relocate(num.var) %>% rename(var = num.var)
  corrtab <- data.frame(lapply(corrtab, as.character), stringsAsFactors=FALSE)
  corrtab[1, -c(1)] <- " "


  ## APA formatting: Get rid of leading 0's

  for(i in 1:nrow(corrtab)){

    corrtab[i, -c(1)] %<>% str_replace("0.", ".")

  }

  ### OPTION 1: Adding descriptives (describe = TRUE; using custom function above) and OPTION 2: changing top labels to numbers only (numbers = TRUE)


  desc.output <- df %>% new.describe(vars)

  if(numbers==TRUE){

    new.col.names <- c("var", 1:(ncol(corrtab)-1))

    colnames(corrtab) <- new.col.names
    colnames(desc.output) <- new.col.names
  } else {


  }

  if(numbers==FALSE & full.labels==TRUE){

    new.col.names <- c("var", names)

    colnames(corrtab) <- new.col.names
    colnames(desc.output) <- new.col.names
  } else {

  }


  if(describe==TRUE){

    corrtab <- dplyr::bind_rows(corrtab, desc.output)

  }else{


  }

  ### OPTION 3: Adding sub headers to organize y labels (up to 3 headers); Add header text to `header = ` option and the row number you would like to place it with the `spots= ` option.

  if(length(headers)==1){

    corrtab %<>% tibble::add_row(var = headers[1], .before = spots[1])
    corrtab[spots[1], -c(1)] <- " "
  }
  else if(length(headers)==2){

    corrtab %<>% tibble::add_row(var = headers[1], .before = spots[1])
    corrtab %<>% tibble::add_row(var = headers[2], .before = spots[2]+1)
    corrtab[spots[1], -c(1)] <- " "
    corrtab[spots[2]+1, -c(1)] <- " "

  } else if(length(headers)==3){

    corrtab %<>% tibble::add_row(var = headers[1], .before = spots[1])
    corrtab %<>% tibble::add_row(var = headers[2], .before = spots[2]+1)
    corrtab %<>% tibble::add_row(var = headers[3], .before = spots[3]+2)
    corrtab[spots[1], -c(1)] <- " "
    corrtab[spots[2]+1, -c(1)] <- " "
    corrtab[spots[3]+2, -c(1)] <- " "

  } else if(length(headers)==4){

    corrtab %<>% tibble::add_row(var = headers[1], .before = spots[1])
    corrtab %<>% tibble::add_row(var = headers[2], .before = spots[2]+1)
    corrtab %<>% tibble::add_row(var = headers[3], .before = spots[3]+2)
    corrtab %<>% tibble::add_row(var = headers[4], .before = spots[4]+3)
    corrtab[spots[1], -c(1)] <- " "
    corrtab[spots[2]+1, -c(1)] <- " "
    corrtab[spots[3]+2, -c(1)] <- " "
    corrtab[spots[4]+3, -c(1)] <- " "

  } else{


  }

  ### OPTION 4: Making the output easily copiable (copy = TRUE) or formatted nicely, but not easily copied (copy = FALSE)

  if(copy==FALSE){

    corrtab %>% kable(format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 12, full_width = F)

  }else{
    corrtab
  }

}



corstarsl.all.kiIN <- function(x, rows=auto, cols=auto){
  require(Hmisc)

  auto = 0

  col.end <- ncol(x)
  col.start <- rows+1
  x <- as.matrix(x)
  R <- rcorr(x)$r
  #R <- R[1:rows, col.start:col.end]
  p <- rcorr(x)$P
  #p <- p[1:rows, col.start:col.end]

  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***",
                    ifelse(p < .01, "**",
                           ifelse(p < .05, "*", # significant
                                  ifelse(p < 0.1, "†", # marginal
                                         ifelse(p < 0.15, ".", " "))))) # trending

  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]


  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=" "), ncol=ncol(x))
  #Rnew <- Rnew[1:rows, col.start:col.end]
  #R <- R[1:rows, col.start:col.end]
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  if(rows!=auto & cols!=auto){
    ## remove lower triangle
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)

    ## remove last column and return the matrix (which is now a data frame)
    #Rnew <- cbind(Rnew[1:length(Rnew)-1])
    Rnew <- Rnew[1:rows, col.start:col.end]

  } else{

    ## remove upper triangle
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)

    ## remove last column and return the matrix (which is now a data frame)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])

  }


  return(Rnew)
}

#' Basic correlation table
#' @param df data frame
#' @param vars quosure of variables to analyze
#' @param rows number of variables used as rows; used in conjunction with cols. Default is all vars
#' @param cols number of variables used as columns; used in conjunction with rowss. Default is all vars
#' @param copy Should this table be 'copiable' or formatted?
#' @export

table.corr <- function(df, vars, rows=auto, cols=auto, copy=FALSE){

  auto = 0

  if(rows!=auto & cols!=auto){

    corrtab <- df %>%
      dplyr::select(!!!vars) %>%
      corstarsl.all.kiIN(., rows, cols)

  } else{

    corrtab <- df %>%
      dplyr::select(!!!vars) %>%
      corstarsl.all.kiIN() %>%
      rownames_to_column(., var = "var") %>%
      # mutate(var = new_row_names(.$var)) %>%
      slice(-1)

  }


  if(copy==FALSE){

    corrtab %>% kable(format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 12, full_width = F)

  }else{
    corrtab
  }

}



######### Partial correlations
corstarsl.all.kiIN3 <- function(data, var.list, des, rows=auto, cols=auto, tri = "lower", round = 3){
  require(Hmisc)

  auto = 0

  x <- data %>% dplyr::select(!!!var.list)
  #vars <- quos(vars)

  #col.end <- ncol(x)-length(des)
  #col.start <- rows+1

  if(class(rows)=="character"& nchar(rows)>1){
    auto <- "no"
    vars <- cbind(rows, cols, des)
    row.l = length(rows)
    col.l = length(cols)
    des.l = length(des)

    col.start <- row.l+1
    col.end <-row.l+col.l

    x <- x %>% dplyr::select(vars)

  } else if(class(rows)=="numeric"& rows>0){
    auto <- 0
    col.end <- ncol(x)-length(des)
    col.start <- rows+1
    row.l <- rows


  } else if (rows==auto){

    auto <- 0
  } else {

    auto <- 0
  }

  n <- as.numeric(nrow(x)-length(des))
  c <- as.numeric(ncol(x)-length(des))
  x <- as.matrix(x)
  R <- psych::partial.r(x, x = c(1:c), y = des)
  p <- psych::corr.p(R, n)$p

  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***",
                    ifelse(p < .01, "**",
                           ifelse(p < .05, "*", # significant
                                  ifelse(p < 0.1, "†",
                                         ifelse(p > 0.10, " ", NA))))) #marginal

  ## trunctuate the matrix that holds the correlations to x number of decimals
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), round))[,-1]


  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste0(R, mystars, sep=" "), ncol=ncol(R))
  diag(Rnew) <- paste(diag(R), " ", sep="")

  ##The following two lines of code name the rows to match the columns, and truncate any name (for rows and cols) longer than 20 characters
  rownames(Rnew) <- ifelse(nchar(colnames(R)) > 20, paste0(substr(colnames(R), start = 1, stop = 20), "..."), colnames(R))
  colnames(Rnew) <- paste(ifelse(nchar(colnames(R)) > 20, paste0(substr(colnames(R), start = 1, stop = 20), "..."), colnames(R)), "", sep="")

  if(rows!=auto & cols!=auto){
    ## remove lower triangle
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)

    ## remove last column and return the matrix (which is now a data frame)
    #Rnew <- cbind(Rnew[1:length(Rnew)-1])
    Rnew <- cbind(Rnew[1:row.l, col.start:col.end])
    #Rnew <- Rnew[1:row.l, col.start:col.end]

  } else if(tri=="lower"){

    ## remove upper triangle
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)

    ## remove last column and return the matrix (which is now a data frame)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])

  } else{

    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew[-c(nrow(Rnew)), ])

    ## remove last column and return the matrix (which is now a data frame)
    #Rnew <- cbind(Rnew[1:length(Rnew)-1])
    Rnew <- cbind.data.frame(Rnew[1:length(Rnew)])

  }



  return(Rnew)
}

#' Partial correlation table
#' @param df data frame of vars to analyse
#' @param control.vars variables to control for in analysis
#' @param rows number of variables used as rows; used in conjunction with cols. Default is all vars
#' @param cols number of variables used as columns; used in conjunction with rowss. Default is all vars
#' @param triangle toggle lower/upper triangle output of correlations
#' @param copy Should this table be 'copiable' or formatted?
#' @export

corrtrol <- function(df, vars, control.vars, rows=auto, cols=auto, names = NULL, triangle = "lower", round = 3, copy=FALSE){

  auto = 0

  if(class(rows)=="character"&nchar(rows)>1){
    auto <- "no"

    corrtab <- df %>%
      corstarsl.all.kiIN3(., vars, control.vars, rows, cols, tri = triangle, round = round)

  } else if(class(rows)=="numeric"&rows>0){
    auto <- 0

    corrtab <- df %>%
      corstarsl.all.kiIN3(., vars, control.vars, rows, cols, tri = triangle, round = round)

  } else {
    auto <- 0

    corrtab <- df %>%
      corstarsl.all.kiIN3(., vars, control.vars, tri = triangle, round = round) %>%
      tibble::rownames_to_column(., var = "var")
    # mutate(var = new_row_names(.$var)) %>%
    # slice(-1)

  }


  if(!is.null(names)){

    if(triangle=="lower"){
      corrtab <- corrtab %<>%
        mutate(var = names) #%>%
      # mutate(var = new_row_names(.$var)) %>%
      #slice(-1)
      colnames(corrtab) <- c("var", names[1:length(names)-1])

    } else if(triangle=="upper"){
      colnames(corrtab) <- c("var", names)

      # corrtab %<>% mutate(
      #
      #   var = names[1:length(names)-1],
      #   .before = .[, 1]
      # )
      corrtab[,1] <- names[1:length(names)-1]

    } else {

      print("Please choose 'triangle = upper' or 'triangle = lower'.")
    }


  } else{

  }




  ## APA formatting: Get rid of leading 0's

  corrtab <- data.frame(lapply(corrtab, as.character), stringsAsFactors=FALSE)
  #corrtab[1, -c(1)] <- " "

  for(i in 1:nrow(corrtab)){

    corrtab[i, -c(1)] %<>% stringr::str_replace("0.", ".")

  }


  if(copy==FALSE){

    corrtab %>% kable(format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 12, full_width = F)

  }else{
    corrtab
  }


}

