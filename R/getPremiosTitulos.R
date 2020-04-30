#' @title getPremiosTitulos
#' @description Extract Awards and Medals from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(lattesXML)
#'  # to import from one curriculum 
#'  getPremiosTitulos(lattesXML[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(lattesXML, getPremiosTitulos)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getPremiosTitulos
#' @export 
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
