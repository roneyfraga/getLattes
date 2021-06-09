#' @title getParticipacaoProjeto
#' @description Extract Participation in Projects from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getParticipacaoProjeto(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getParticipacaoProjeto)
#'  head(bind_rows(lt))
#'  }
#' @rdname getParticipacaoProjeto
#' @export 
#' @importFrom pipeR "%>>%"
getParticipacaoProjeto <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//PROJETO-DE-PESQUISA")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//EQUIPE-DO-PROJETO")) %>>%
        purrr::map(~ xml2::xml_children(.)) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> equipe)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//FINANCIADORES-DO-PROJETO")) %>>%
        purrr::map(~ xml2::xml_children(.)) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> financiadores)

    pmap(list(dados_basicos, equipe), function(x, y) tibble::tibble(x, equipe = list(y))) %>>%
        (purrr::pmap(list(., financiadores), function(x, y) tibble::tibble(x, financiadores = list(y)))) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 
}
