#' @title getFormacaoDoutorado
#' @description Extract Profissional Formation from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return list
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getFormacao(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getFormacao)
#'  }
#' @rdname getFormacaoDoutorado
#' @export 
#' @importFrom pipeR "%>>%"
getFormacaoDoutorado <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//FORMACAO-ACADEMICA-TITULACAO/DOUTORADO") %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> doutorado)

    xml2::xml_find_all(curriculo, ".//FORMACAO-ACADEMICA-TITULACAO/DOUTORADO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) %>>%
        purrr::map(~ xml2::xml_children(.)) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> doutorado_area)

    if (nrow(doutorado_area[[1]]) == 0) doutorado_area <- tibble::tibble(nome_grande_area_do_conhecimento = NA, 
                                                                         nome_da_area_do_conhecimento = NA, 
                                                                         nome_da_sub_area_do_conhecimento = NA, 
                                                                         nome_da_especialidade = NA)

    doutorado  %>>%  
        (purrr::pmap(list(., doutorado_area), function(x, y) tibble::tibble(x, area = list(y)))) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}
