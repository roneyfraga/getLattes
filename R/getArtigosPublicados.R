#' @title getArtigosPublicados
#' @description Extract published papers from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getArtigosPublicados(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getArtigosPublicados)
#'  head(bind_rows(lt))
#'  }
#' @rdname getArtigosPublicados
#' @export 
#' @importFrom pipeR "%>>%"
getArtigosPublicados <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//ARTIGO-PUBLICADO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DO-ARTIGO")) %>>%
        purrr::map(~ xml2::xml_attrs()) %>>%
        purrr::map(~ dplyr::bind_rows()) %>>%
        purrr::map(~ janitor::clean_names()) %>>%
        (. -> dados_basicos)

    xml2::xml_find_all(curriculo, ".//ARTIGO-PUBLICADO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DO-ARTIGO")) %>>%
        purrr::map(~ xml2::xml_attrs()) %>>%
        purrr::map(~ dplyr::bind_rows()) %>>%
        purrr::map(~ janitor::clean_names()) %>>%
        (. -> detalhamento)

    xml2::xml_find_all(curriculo, ".//ARTIGO-PUBLICADO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//AUTORES")) %>>%
        purrr::map(~ xml2::xml_attrs()) %>>%
        purrr::map(~ dplyr::bind_rows()) %>>%
        purrr::map(~ janitor::clean_names()) %>>%
        (. -> autores)

    purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) %>>%
        (purrr::pmap(list(., autores), function(x, y) tibble::tibble(x, autores = list(y)))) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 
}
