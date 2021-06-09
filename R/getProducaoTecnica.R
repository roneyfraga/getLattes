#' @title getProducaoTecnica
#' @description Extract Technical Production from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getProducaoTecnica(curriculo)
#'
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[janitor]{clean_names}}
#'  \code{\link[tibble]{tibble}}
#' @rdname getProducaoTecnica
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom purrr map map2 pmap
#' @importFrom dplyr bind_rows mutate select bind_cols
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
#' @importFrom pipeR "%>>%"
getProducaoTecnica <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml2::xml_find_all(curriculo, ".//TRABALHO-TECNICO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DO-TRABALHO-TECNICO")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml2::xml_find_all(curriculo, ".//TRABALHO-TECNICO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DO-TRABALHO-TECNICO")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    xml2::xml_find_all(curriculo, ".//TRABALHO-TECNICO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//AUTORES")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        (. -> autores)

    xml2::xml_find_all(curriculo, ".//TRABALHO-TECNICO") %>>%
        purrr::map(~ xml2::xml_find_all(., ".//PALAVRAS-CHAVE")) %>>%
        purrr::map(~ xml2::xml_attrs(.)) %>>%
        purrr::map(~ dplyr::bind_rows(.)) %>>%
        purrr::map(~ janitor::clean_names(.)) %>>%
        purrr::map(~ .x %>>% dplyr::mutate(palavras_chave = paste(., collapse = ';'))) %>>%
        purrr::map(~ .x %>>% dplyr::select(palavras_chave)) %>>%
        (. -> palavras_chave)

    purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) %>>%
        purrr::map2(palavras_chave, dplyr::bind_cols) %>>%
        (purrr::pmap(list(., autores), function(x, y) tibble::tibble(x, autores = list(y)))) %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 
}
