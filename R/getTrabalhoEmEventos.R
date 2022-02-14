#' @title getTrabalhosEmEventos
#' @description Extract published papers from 'Lattes' XML file. 
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#' 
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getTrabalhosEmEventos(curriculo)
#'
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_attr}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{mutate}}
#'  \code{\link[janitor]{clean_names}}
#'  \code{\link[tibble]{tibble}}
#' @rdname getTrabalhosEmEventos
#' @export 
#' @importFrom xml2 xml_find_all xml_attrs
#' @importFrom purrr map map2 pmap
#' @importFrom dplyr bind_rows bind_cols mutate
#' @importFrom janitor clean_names
getTrabalhosEmEventos <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    dados_basicos <- 
        curriculo |>
            xml2::xml_find_all(".//TRABALHO-EM-EVENTOS") |> 
            purrr::map(~ xml2::xml_find_all(., ".//DADOS-BASICOS-DO-TRABALHO")) |>
            purrr::map(~ xml2::xml_attrs(.)) |>
            purrr::map(~ dplyr::bind_rows(.)) |>
            purrr::map(~ janitor::clean_names(.)) 

    detalhamento <- 
        curriculo |>
            xml2::xml_find_all(".//TRABALHO-EM-EVENTOS") |>
            purrr::map(~ xml2::xml_find_all(., ".//DETALHAMENTO-DO-TRABALHO")) |>
            purrr::map(~ xml2::xml_attrs(.)) |>
            purrr::map(~ dplyr::bind_rows(.)) |>
            purrr::map(~ janitor::clean_names(.)) 

    autores <- 
        curriculo |> 
            xml2::xml_find_all(".//TRABALHO-EM-EVENTOS") |>
            purrr::map(~ xml2::xml_find_all(., ".//AUTORES")) |>
            purrr::map(~ xml2::xml_attrs(.)) |>
            purrr::map(~ dplyr::bind_rows(.)) |>
            purrr::map(~ janitor::clean_names(.)) 

    a <- purrr::map2(dados_basicos, detalhamento, dplyr::bind_cols) 

    purrr::pmap(list(a, autores), function(x, y) tibble::tibble(x, autores = list(y))) |>
        dplyr::bind_rows() |>
        dplyr::mutate(id = getId(curriculo)) 
}
