#' @title getDadosGerais
#' @description Extract General Data from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(lattesXML)
#'  # to import from one curriculum 
#'  getDadosGerais(lattesXML[[999]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(lattesXML, getDadosGerais)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getDadosGerais
#' @export 
#' @importFrom dplyr mutate_if
getDadosGerais <- function(curriculo){
    #print(curriculo$id)
    ll <- curriculo$`DADOS-GERAIS`
    if(any('NOME-COMPLETO' %in% names(ll$.attrs))){
        if(length(ll)>1){
            dados.gerais <- .getCharacter(ll$.attrs)
            dados.gerais$id <- curriculo$id
            dados.gerais$data.atualizacao <- curriculo$.attrs[['DATA-ATUALIZACAO']]
            dados.gerais <- mutate_if(dados.gerais, is.factor, as.character)
        } else { dados.gerais <- NULL }
    } else { dados.gerais <- NULL }
    return(dados.gerais)
}
