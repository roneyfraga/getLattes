#' @title getOutrasProducoesTecnicas
#' @description Extract Other Technical Productions from 'Lattes' XML file.
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getOutrasProducoesTecnicas(curriculo)
#'  
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}},\code{\link[xml2]{xml_children}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#'  \code{\link[tibble]{tibble}}
#' @rdname getOutrasProducoesTecnicas
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs xml_children
#' @importFrom purrr map map2 pmap
#' @importFrom dplyr bind_rows bind_cols mutate
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
getOutrasProducoesTecnicas <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    dados_basicos <- 
        xml2::xml_find_all(curriculo, ".//OUTRA-PRODUCAO-TECNICA") |>
        purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA")) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) 

    detalhamento <- 
        xml2::xml_find_all(curriculo, ".//OUTRA-PRODUCAO-TECNICA") |>
        purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DE-OUTRA-PRODUCAO-TECNICA")) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) 

    autores <- 
        xml2::xml_find_all(curriculo, ".//OUTRA-PRODUCAO-TECNICA") |>
        purrr::map(~ xml2::xml_find_all(., ".//AUTORES")) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) 

    areas_conhecimento <- 
        xml2::xml_find_all(curriculo, ".//OUTRA-PRODUCAO-TECNICA") |>
        purrr::map(~ xml2::xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) |>
        purrr::map(~ xml2::xml_children(.)) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) |>
        purrr::map(~ if (nrow(.x) == 0) { tibble::tibble(areas_conhecimento = NA) } else {.x})  

    a <- 
        purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols)

    b <- 
        purrr::pmap(list(a, autores), function(x, y) tibble::tibble(x, autores = list(y))) 

    purrr::pmap(list(b, areas_conhecimento), function(x, y) tibble::tibble(x, areas_conhecimento = list(y))) |>
        dplyr::bind_rows() |>
        dplyr::mutate(id = getId(curriculo))  
}
