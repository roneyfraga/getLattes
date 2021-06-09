#' @title getBancasDoutorado
#' @description Extract Ph.D. Examination Board's from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getBancasDoutorado(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getBancasDoutorado)
#'  head(bind_rows(lt))
#'  }
#' @rdname getBancasDoutorado
#' @export 
#' @importFrom pipeR "%>>%"
getBancasDoutorado <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-DOUTORADO") %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> sequencia)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-DOUTORADO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-DOUTORADO")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-DOUTORADO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DA-PARTICIPACAO-EM-BANCA-DE-DOUTORADO")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-DOUTORADO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//PARTICIPANTE-BANCA")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> participantes)

    purrr::map2(sequencia, dados_basicos, dplyr::bind_cols) %>>%
        purrr::map2(detalhamento, dplyr::bind_cols) %>>%
        (purrr::pmap(list(., participantes), function(x, y) tibble::tibble(x, participantes = list(y)))) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}
