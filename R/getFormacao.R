#' @title getFormacao
#' @description Extract Profissional Formation from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return list
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getFormacao(xmlsLattes[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getFormacao)
#'  }
#' }
#' @rdname getFormacao
#' @export 
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
