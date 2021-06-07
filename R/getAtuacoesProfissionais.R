#' @title getAtuacoesProfissionais
#' @description Extract profissional links from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getAtuacoesProfissionais(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getAtuacoesProfissionais)
#'  head(bind_rows(lt))
#'  }
#' @rdname getAtuacoesProfissionais
#' @export 
#' @importFrom stringr str_c
getAtuacoesProfissionais <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//ATUACOES-PROFISSIONAIS/ATUACAO-PROFISSIONAL") %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml_find_all(curriculo, ".//ATUACOES-PROFISSIONAIS/ATUACAO-PROFISSIONAL") %>>%
        map(~ xml_find_all(., ".//VINCULOS")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    map2(dados_basicos, detalhamento, bind_cols) %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}
