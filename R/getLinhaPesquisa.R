#' @title getLinhaPesquisa
#' @description Extract Research Lines from 'Lattes' XML file. 
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml()('file.xml')
#'  # getLinhaPesquisa(curriculo)
#'  
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#' @rdname getLinhaPesquisa
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom purrr map map2
#' @importFrom dplyr bind_rows bind_cols mutate
#' @importFrom janitor clean_names
#' @importFrom pipeR "%>>%"
getLinhaPesquisa <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//PESQUISA-E-DESENVOLVIMENTO") %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml2::xml_find_all(curriculo, ".//PESQUISA-E-DESENVOLVIMENTO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//LINHA-DE-PESQUISA")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 
}
