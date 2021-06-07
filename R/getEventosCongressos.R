#' @title getEventosCongressos
#' @description Extract Events and Congresses from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getEventosCongressos(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getEventosCongressos)
#'  head(bind_rows(lt))
#'  }
#' @rdname getEventosCongressos
#' @export 

getEventosCongressos <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }


    xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        map(~ xml_find_all(., ".//DADOS-BASICOS-DA-PARTICIPACAO-EM-CONGRESSO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)
        
    xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        map(~ xml_find_all(., ".//DETALHAMENTO-DA-PARTICIPACAO-EM-CONGRESSO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        map(~ xml_find_all(., ".//PARTICIPANTE-DE-EVENTOS-CONGRESSOS")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> participantes)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        map(~ xml_find_all(., ".//PALAVRAS-CHAVE")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        map(~ .x %>>% dplyr::mutate(palavras_chave = paste(., collapse = ';'))) %>>%
        map(~ .x %>>% dplyr::select(palavras_chave)) %>>%
        (. -> palavras_chave)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-CONGRESSO") %>>%
        map(~ xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) %>>%
        map(~ xml_children(.)) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        map(~ if (nrow(.x) == 0) { tibble(areas_conhecimento = NA) } else {.x})  %>>%
        (. -> areas_conhecimento)

    map2(dados_basicos, detalhamento, bind_cols) %>>%
    map2(palavras_chave, bind_cols) %>>%
        (pmap(list(., participantes), function(x, y) tibble(x, participantes = list(y)))) %>>%
        (pmap(list(., areas_conhecimento), function(x, y) tibble(x, areas_conhecimento = list(y)))) %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo))
}
