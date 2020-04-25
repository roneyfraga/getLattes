#' @title getFormacao
getFormacao <- function(curriculo){
  #print(curriculo$id)
  ll <- curriculo$`DADOS-GERAIS`
  if(any( names(ll) %in% 'FORMACAO-ACADEMICA-TITULACAO')){

    ll <- ll$`FORMACAO-ACADEMICA-TITULACAO`

    if(!is.null(ll)){
      formacao <- lapply(ll, function(x){ if(!is.null(x)){ .getCharacter(x)} } )
      formacao$id <- curriculo$id
      return(formacao)
    }else{
      formacao <- NULL
      return(formacao)
    }
  } else{
    formacao <- NULL
    return(formacao)
  }
}
