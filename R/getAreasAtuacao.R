#' @title getAreasAtuacao
#' @description Extract Research Area from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(lattesXML)
#'  # to import from one curriculum 
#'  getAreasAtuacao(lattesXML[[999]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(lattesXML, getAreasAtuacao)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getAreasAtuacao
#' @export 
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
