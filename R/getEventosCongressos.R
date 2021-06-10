#' @title getEventosCongressos
#' @description Extract Events and Congresses from 'Lattes' XML file.
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getEventosCongressos(curriculo)
#'  
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}},\code{\link[xml2]{xml_children}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[janitor]{clean_names}}
#'  \code{\link[tibble]{tibble}}
#' @rdname getEventosCongressos
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs xml_children
#' @importFrom purrr map map2 pmap
#' @importFrom dplyr bind_rows mutate select bind_cols
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
getEventosCongressos <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    dados_basicos <- 
        xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") |>
        purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DA-PARTICIPACAO-EM-CONGRESSO")) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) 

    detalhamento <- 
        xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") |>
        purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DA-PARTICIPACAO-EM-CONGRESSO")) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) 

    participantes <- 
        xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") |>
        purrr::map(~ xml2::xml_find_all(., ".//PARTICIPANTE-DE-EVENTOS-CONGRESSOS")) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.))

    palavras_chave <- 
        xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") |>
        purrr::map(~ xml2::xml_find_all(., ".//PALAVRAS-CHAVE")) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) |>
        purrr::map(~ .x |> dplyr::mutate(palavras_chave = paste(., collapse = ';'))) |>
        purrr::map(~ .x |> dplyr::select(palavras_chave)) 

    areas_conhecimento <- 
        xml2::xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") |>
        purrr::map(~ xml2::xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) |>
        purrr::map(~ xml2::xml_children(.)) |>
        purrr::map(~ xml2::xml_attrs(.)) |>
        purrr::map(~ dplyr::bind_rows(.)) |>
        purrr::map(~ janitor::clean_names(.)) |>
        purrr::map(~ if (nrow(.x) == 0) { tibble::tibble(areas_conhecimento = NA) } else {.x})  

    a <- 
        purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) |>
        purrr::map2(palavras_chave, dplyr::bind_cols) 

    b <- 
        purrr::pmap(list(a, participantes), function(x, y) tibble::tibble(x, participantes = list(y))) 

    purrr::pmap(list(b, areas_conhecimento), function(x, y) tibble::tibble(x, areas_conhecimento = list(y))) |>
        dplyr::bind_rows() |>
        dplyr::mutate(id = getId(curriculo))
}
