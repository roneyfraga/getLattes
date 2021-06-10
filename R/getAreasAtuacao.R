#' @title getAreasAtuacao
#' @description Extract Research Area from 'Lattes' XML file.
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getAreasAtuacao(curriculo)
#'
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#' @rdname getAreasAtuacao
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @importFrom pipeR "%>>%"
getAreasAtuacao <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//AREAS-DE-ATUACAO/AREA-DE-ATUACAO") %>>%
        xml2::xml_attrs() %>>%
        dplyr::bind_rows() %>>%
        janitor::clean_names() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}
