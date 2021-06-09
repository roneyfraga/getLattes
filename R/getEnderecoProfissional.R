#' @title getEnderecoProfissional
#' @description Extract Profissional Address from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getEnderecoProfissional(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getEnderecoProfissional)
#'  head(bind_rows(lt))
#'  }
#' @rdname getEnderecoProfissional
#' @export 
#' @importFrom pipeR "%>>%"
getEnderecoProfissional <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//ENDERECO-PROFISSIONAL") %>>%
        xml2::xml_attrs() %>>%
        dplyr::bind_rows() %>>%
        janitor::clean_names() %>>%
        dplyr::mutate(id = getId(curriculo))
}

