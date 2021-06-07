#' @title getProducaoTecnica
#' @description Extract Technical Production from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getProducaoTecnica(xmlsLattes[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getProducaoTecnica)
#'  head(bind_rows(lt))
#'  }
#' @rdname getProducaoTecnica
#' @export 
getProducaoTecnica <- function(curriculo) {
    
    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//TRABALHO-TECNICO") %>>%
        map(~ xml_find_all(., ".//DADOS-BASICOS-DO-TRABALHO-TECNICO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml_find_all(curriculo, ".//TRABALHO-TECNICO") %>>%
        map(~ xml_find_all(., ".//DETALHAMENTO-DO-TRABALHO-TECNICO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    xml_find_all(curriculo, ".//TRABALHO-TECNICO") %>>%
        map(~ xml_find_all(., ".//AUTORES")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> autores)

    xml_find_all(curriculo, ".//TRABALHO-TECNICO") %>>%
        map(~ xml_find_all(., ".//PALAVRAS-CHAVE")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        map(~ .x %>>% dplyr::mutate(palavras_chave = paste(., collapse = ';'))) %>>%
        map(~ .x %>>% dplyr::select(palavras_chave)) %>>%
        (. -> palavras_chave)

    map2(dados_basicos, detalhamento, bind_cols) %>>%
    map2(palavras_chave, bind_cols) %>>%
        (pmap(list(., autores), function(x, y) tibble(x, autores = list(y)))) %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 
}
