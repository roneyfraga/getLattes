#' @title getAreasAtuacao
getAreasAtuacao <- function(curriculo){
  #print(curriculo$id)
  ll <- curriculo$`DADOS-GERAIS`
  if(any(names(ll) %in% 'AREAS-DE-ATUACAO')){

    ll2 <- ll$`AREAS-DE-ATUACAO`

    if(!is.null(ll2)){

      if(length(ll2)>=1){
        arat <- lapply(ll2, function(x){ if(!is.null(x)){ .getCharacter(x)} } )
        arat <- bind_rows(arat)
      }
      arat$id <- curriculo$id
      return(arat)
    }
  } else{
    arat <- NULL
    return(arat)
  }
}
