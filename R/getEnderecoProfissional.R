#' @title getEnderecoProfissional
getEnderecoProfissional <- function(curriculo){
  #print(curriculo$id)
  ll <- curriculo$`DADOS-GERAIS`$ENDERECO
  if(any('ENDERECO-PROFISSIONAL' %in% names(ll))){
    ll <- ll$`ENDERECO-PROFISSIONAL`
    if(length(ll)>1){
      endereco <- as.data.frame(t(ll))
      names(endereco) <- .vname(endereco)
      endereco$id <- curriculo$id
    } else { endereco <- NULL }
    return(endereco)
  } else { endereco <- NULL }
  return(endereco)
}
