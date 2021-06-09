#' @title getOutrasProducoesTecnicas
#' @description Extract Other Technical Productions from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getOutrasProducoesTecnicas(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getOutrasProducoesTecnicas)
#'  head(bind_rows(lt))
#'  }
#' @rdname getOutrasProducoesTecnicas
#' @export 
#' @importFrom pipeR "%>>%"
getOutrasProducoesTecnicas <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//OUTRA-PRODUCAO-TECNICA") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA")) %>>%
        purrr::map(~ xml2::xml_attrs()) %>>%
        purrr::map(~ dplyr::bind_rows()) %>>%
        purrr::map(~ janitor::clean_names()) %>>%
        (. -> dados_basicos)

    xml2::xml_find_all(curriculo, ".//OUTRA-PRODUCAO-TECNICA") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DE-OUTRA-PRODUCAO-TECNICA")) %>>%
        purrr::map(~ xml2::xml_attrs()) %>>%
        purrr::map(~ dplyr::bind_rows()) %>>%
        purrr::map(~ janitor::clean_names()) %>>%
        (. -> detalhamento)

    xml2::xml_find_all(curriculo, ".//OUTRA-PRODUCAO-TECNICA") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//AUTORES")) %>>%
        purrr::map(~ xml2::xml_attrs()) %>>%
        purrr::map(~ dplyr::bind_rows()) %>>%
        purrr::map(~ janitor::clean_names()) %>>%
        (. -> autores)

    xml2::xml_find_all(curriculo, ".//OUTRA-PRODUCAO-TECNICA") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) %>>%
        purrr::map(~ xml2::xml_children()) %>>%
        purrr::map(~ xml2::xml_attrs()) %>>%
        purrr::map(~ dplyr::bind_rows()) %>>%
        purrr::map(~ janitor::clean_names()) %>>%
        purrr::map(~ if (nrow(.x) == 0) { tibble::tibble(areas_conhecimento = NA) } else {.x})  %>>%
        (. -> areas_conhecimento)

    purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) %>>%
        (purrr::pmap(list(., autores), function(x, y) tibble::tibble(x, autores = list(y)))) %>>%
        (purrr::pmap(list(., areas_conhecimento), function(x, y) tibble::tibble(x, areas_conhecimento = list(y)))) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo))  
}
