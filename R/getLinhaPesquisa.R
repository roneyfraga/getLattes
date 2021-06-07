#' @title getLinhaPesquisa
#' @description Extract Research Lines from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getLinhaPesquisa(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getLinhaPesquisa)
#'  head(bind_rows(lt))
#'  }
#' @rdname getLinhaPesquisa
#' @export 
getLinhaPesquisa <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//PESQUISA-E-DESENVOLVIMENTO") %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml_find_all(curriculo, ".//PESQUISA-E-DESENVOLVIMENTO") %>>%
        map(~ xml_find_all(., ".//LINHA-DE-PESQUISA")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    map2(dados_basicos, detalhamento, bind_cols) %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 
}
