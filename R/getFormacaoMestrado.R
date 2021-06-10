#' @title getFormacaoMestrado
#' @description Extract Profissional Formation from 'Lattes' XML file.
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getFormacaoMestrado(curriculo)
#'  
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}},\code{\link[xml2]{xml_children}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#'  \code{\link[tibble]{tibble}}
#' @rdname getFormacaoMestrado
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs xml_children
#' @importFrom purrr map pmap
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
#' @importFrom pipeR "%>>%"
getFormacaoMestrado <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//FORMACAO-ACADEMICA-TITULACAO/MESTRADO") %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> mestrado)

    xml2::xml_find_all(curriculo, ".//FORMACAO-ACADEMICA-TITULACAO/MESTRADO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) %>>%
        purrr::map(~ xml2::xml_children(.)) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> mestrado_area)

    if (nrow(mestrado_area[[1]]) == 0) mestrado_area <- tibble::tibble(nome_grande_area_do_conhecimento = NA, 
                                                                       nome_da_area_do_conhecimento = NA, 
                                                                       nome_da_sub_area_do_conhecimento = NA, 
                                                                       nome_da_especialidade = NA)

    mestrado  %>>%  
        (purrr::pmap(list(., mestrado_area), function(x, y) tibble::tibble(x, area = list(y)))) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}

