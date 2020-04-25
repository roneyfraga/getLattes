#' @title getPremiosTitulos
getPremiosTitulos <- function(curriculo){
  #print(curriculo$id)
  ll <- curriculo$`DADOS-GERAIS`
  if(any( names(ll) %in% 'PREMIOS-TITULOS')){

    ll <- ll$`PREMIOS-TITULOS`

    if(length(ll)>=1){
      pret <- lapply(ll, function(x){ if(!is.null(x)){ .getCharacter(x)} } )
      pret <- bind_rows(pret)
    }
    pret$id <- curriculo$id
    return(pret)
  } else{
    pret <- NULL
    return(pret)
  }
}
