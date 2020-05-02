
#' @title getCharacter
#' @description Extract variable names in import process (get functions).
#' @param x XML exported from Lattes imported to R as list.
#' @return data frame. 
getCharacter <- function(x){
  x %>>%
    as.list(.) %>>%
    (do.call(rbind, .)) %>>%
    (data.frame(t(.))) -> df

  names(df) = tolower(names(df))
  return(df)
}

#' @title mostFrequent
#' @description Extract the most frequent ocorrence, helpfull to group_by.
#' @param x data frame.
#' @return data frame. 
mostFrequent <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",":="))
