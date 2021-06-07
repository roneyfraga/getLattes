#' @title getParticipacaoProjeto
#' @description Extract Participation in Projects from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
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
getParticipacaoProjeto <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") %>>%
        map(~ xml_find_all(., ".//PROJETO-DE-PESQUISA")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)
        
    xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") %>>%
        map(~ xml_find_all(., ".//EQUIPE-DO-PROJETO")) %>>%
        map(~ xml_children(.)) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> equipe)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-PROJETO") %>>%
        map(~ xml_find_all(., ".//FINANCIADORES-DO-PROJETO")) %>>%
        map(~ xml_children(.)) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> financiadores)

    pmap(list(dados_basicos, equipe), function(x, y) tibble(x, equipe = list(y))) %>>%
        (pmap(list(., financiadores), function(x, y) tibble(x, financiadores = list(y)))) %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 
}
