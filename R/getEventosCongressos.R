#' @title getEventosCongressos
#' @description Extract Events and Congresses from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getEventosCongressos(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getEventosCongressos)
#'  head(bind_rows(lt))
#'  }
#' @rdname getEventosCongressos
#' @export 
#' @importFrom pipeR "%>>%"
getEventosCongressos <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }


    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DA-PARTICIPACAO-EM-CONGRESSO")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DA-PARTICIPACAO-EM-CONGRESSO")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//PARTICIPANTE-DE-EVENTOS-CONGRESSOS")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> participantes)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//PALAVRAS-CHAVE")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        purrr::map(~ .x %>>% dplyr::mutate(palavras_chave = paste(., collapse = ';'))) %>>%
        purrr::map(~ .x %>>% dplyr::select(palavras_chave)) %>>%
        (. -> palavras_chave)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) %>>%
        purrr::map(~ xml2::xml_children(.)) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        purrr::map(~ if (nrow(.x) == 0) { tibble::tibble(areas_conhecimento = NA) } else {.x})  %>>%
        (. -> areas_conhecimento)

    purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) %>>%
        purrr::map2(palavras_chave, dplyr::bind_cols) %>>%
        (purrr::pmap(list(., participantes), function(x, y) tibble::tibble(x, participantes = list(y)))) %>>%
        (purrr::pmap(list(., areas_conhecimento), function(x, y) tibble::tibble(x, areas_conhecimento = list(y)))) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo))
}
