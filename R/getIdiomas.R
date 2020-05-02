#' @title getIdiomas
#' @description Extract Languages from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getIdiomas(xmlsLattes[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getIdiomas)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getIdiomas
#' @export 
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
