#' @title getFormacaoGraduacao
#' @description Extract Profissional Formation from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return list
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getFormacao(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getFormacao)
#'  }
#' @rdname getFormacaoGraduacao
#' @export 
getFormacaoGraduacao <- function(curriculo) {

    if(!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//FORMACAO-ACADEMICA-TITULACAO/GRADUACAO") %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> graduacao)

    graduacao %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}
