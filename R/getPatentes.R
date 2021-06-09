#' @title getPatentes
#' @description Extract Patent data from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getProducaoTecnica(xmlsLattes[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getProducaoTecnica)
#'  head(bind_rows(lt))
#'  }
#' @rdname getPatentes
#' @export 
#' @importFrom pipeR "%>>%"
getPatentes <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//DADOS-BASICOS-DA-PATENTE") %>>%
        xml2::xml_attrs() %>>%
        dplyr::bind_rows() %>>%
        janitor::clean_names() %>>%
        dplyr::distinct(.keep_all = TRUE) %>>%
        dplyr::mutate(id = getId(curriculo))
}
