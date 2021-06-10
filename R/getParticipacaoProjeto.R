#' @title getParticipacaoProjeto
#' @description Extract Participation in Projects from 'Lattes' XML file. 
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getParticipacaoProjeto(curriculo)
#'
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}},\code{\link[xml2]{xml_children}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#'  \code{\link[tibble]{tibble}}
#' @rdname getParticipacaoProjeto
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs xml_children
#' @importFrom purrr map pmap
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
getParticipacaoProjeto <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    dados_basicos <- 
        xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") |>
        purrr::map(~ xml2::xml_find_all(., ".//PROJETO-DE-PESQUISA")) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) 

    equipe <- 
        xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") |>
        purrr::map(~ xml2::xml_find_all(., ".//EQUIPE-DO-PROJETO")) |>
        purrr::map(~ xml2::xml_children(.)) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) 

    financiadores <- 
        xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") |>
        purrr::map(~ xml2::xml_find_all(., ".//FINANCIADORES-DO-PROJETO")) |>
        purrr::map(~ xml2::xml_children(.)) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) 

    a <- 
        pmap(list(dados_basicos, equipe), function(x, y) tibble::tibble(x, equipe = list(y))) 

    purrr::pmap(list(a, financiadores), function(x, y) tibble::tibble(x, financiadores = list(y))) |>
        dplyr::bind_rows() |>
        dplyr::mutate(id = getId(curriculo)) 
}
