#' @title getBancasDoutorado
#' @description Extract Ph.D. Examination Board's from 'Lattes' XML file.
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getBancasDoutorado(curriculo)
#'
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#'  \code{\link[tibble]{tibble}}
#' @rdname getBancasDoutorado
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom purrr map map2 pmap
#' @importFrom dplyr bind_rows bind_cols mutate
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
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
