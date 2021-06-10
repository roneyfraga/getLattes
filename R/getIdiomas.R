#' @title getIdiomas
#' @description Extract Languages from 'Lattes' XML file. 
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getIdiomas(curriculo)
#'  
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_children}},\code{\link[xml2]{xml_attr}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#' @rdname getIdiomas
#' @export 
#' @importFrom xml2 xml_find_all xml_children xml_attrs
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @importFrom pipeR "%>>%"
getIdiomas <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//IDIOMAS") %>>%
        purrr::map(~ xml2::xml_children(.)) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}
