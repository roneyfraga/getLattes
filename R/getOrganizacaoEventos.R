#' @title getOrganizacaoEventos
#' @description Extract Event's Organization from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getOrganizacaoEvento(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getOrganizacaoEvento)
#'  head(bind_rows(lt))
#'  }
#' @rdname getOrganizacaoEventos
#' @export 
getOrganizacaoEventos <- function(curriculo) {

    xml_find_all(curriculo, ".//ORGANIZACAO-DE-EVENTO") %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml_find_all(curriculo, ".//ORGANIZACAO-DE-EVENTO") %>>%
        map(~ xml_find_all(., ".//DETALHAMENTO-DA-ORGANIZACAO-DE-EVENTO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    xml_find_all(curriculo, ".//ORGANIZACAO-DE-EVENTO") %>>%
        map(~ xml_find_all(., ".//AUTORES")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> autores)

    map2(dados_basicos, detalhamento, bind_cols) %>>%
        (pmap(list(., autores), function(x, y) tibble(x, autores = list(y)))) %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}
