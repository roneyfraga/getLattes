#' @title getIdiomas
getIdiomas <- function(curriculo){
  #print(curriculo$id)
  ll <- curriculo$`DADOS-GERAIS`
  if(is.list(ll)){
    if(any(names(ll) %in% 'IDIOMAS')){

      ll <- ll$`IDIOMAS`

      if(length(ll) >=1){
        idm <- lapply(ll, function(x){ if(!is.null(x)){ .getCharacter(x)} } )
        idm <- bind_rows(idm)
      }
      idm$id <- curriculo$id
      return(idm)
    }
    idm <- NULL
    return(idm)
  } else{
    idm <- NULL
    return(idm)
  }
}
