#' @title getEnderecoProfissional
#' @description Extract Profissional Address from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getEnderecoProfissional(xmlsLattes[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getEnderecoProfissional)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getEnderecoProfissional
#' @export 
getEnderecoProfissional <- function(curriculo){
  #print(curriculo$id)
  ll <- curriculo$`DADOS-GERAIS`$ENDERECO
  if(any('ENDERECO-PROFISSIONAL' %in% names(ll))){
    ll <- ll$`ENDERECO-PROFISSIONAL`
    if(length(ll)>1){
      endereco <- as.data.frame(t(ll))
      names(endereco) <- tolower(gsub('-','\\.', names(endereco)))
      endereco$id <- curriculo$id
    } else { endereco <- NULL }
    return(endereco)
  } else { endereco <- NULL }
  return(endereco)
}
