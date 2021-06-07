#' @title getDadosGerais
#' @description Extract General Data from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getDadosGerais(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getDadosGerais)
#'  head(bind_rows(lt))
#'  }
#' @rdname getDadosGerais
#' @export 
#' @importFrom dplyr mutate_if
getDadosGerais <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//DADOS-GERAIS") %>>%
        xml_attrs() %>>%
        bind_rows() %>>%
        janitor::clean_names() %>>%
        mutate(id = getId(curriculo)) 

}
