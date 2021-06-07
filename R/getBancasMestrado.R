#' @title getBancasMestrado
#' @description Extract Master Examination Board's from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getBancasMestrado(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getBancasMestrado)
#'  head(bind_rows(lt))
#'  }
#' @rdname getBancasMestrado
#' @export 
getBancasMestrado <- function(curriculo) {

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-MESTRADO") %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> sequencia)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-MESTRADO") %>>%
        map(~ xml_find_all(., ".//DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-MESTRADO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-MESTRADO") %>>%
        map(~ xml_find_all(., ".//DETALHAMENTO-DA-PARTICIPACAO-EM-BANCA-DE-MESTRADO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-MESTRADO") %>>%
        map(~ xml_find_all(., ".//PARTICIPANTE-BANCA")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> participantes)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-MESTRADO") %>>%
        map(~ xml_find_all(., ".//PALAVRAS-CHAVE")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        map(~ .x %>>% dplyr::mutate(palavras_chave = paste(., collapse = ';'))) %>>%
        map(~ .x %>>% dplyr::select(palavras_chave)) %>>%
        (. -> palavras_chave)

    xml_find_all(curriculo, ".//PARTICIPACAO-EM-BANCA-DE-MESTRADO") %>>%
        map(~ xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) %>>%
        map(~ xml_children(.)) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> area_do_conhecimento)

    map2(sequencia, dados_basicos, bind_cols) %>>%
    map2(detalhamento, bind_cols) %>>%
    map2(palavras_chave, bind_cols) %>>%
    (pmap(list(., participantes), function(x, y) tibble(x, participantes = list(y)))) %>>%
    (pmap(list(., area_do_conhecimento), function(x, y) tibble(x, area_do_conhecimento = list(y)))) %>>%
    bind_rows() %>>%
    dplyr::mutate(id = getId(curriculo)) 

}
