#' @title getPatentes
#' @description Extract Patent data from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getPatentes(curriculo)
#'
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#' @rdname getPatentes
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom dplyr bind_rows distinct mutate
#' @importFrom janitor clean_names
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
