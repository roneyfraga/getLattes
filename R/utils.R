
#' @title .getCharacter
.getCharacter <- function(x){
  x %>>%
    as.list(.) %>>%
    (do.call(rbind, .)) %>>%
    (data.frame(t(.))) -> df

  names(df) = tolower(names(df))
  return(df)
}

#' @title mostFrequent
mostFrequent <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

