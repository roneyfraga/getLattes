#' @title getFormacaoDoutorado
#' @description Extract Profissional Formation from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return list
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getFormacao(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getFormacao)
#'  }
#' @rdname getFormacaoDoutorado
#' @export 
getFormacaoDoutorado <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
            stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//FORMACAO-ACADEMICA-TITULACAO/DOUTORADO") %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> doutorado)

    xml_find_all(curriculo, ".//FORMACAO-ACADEMICA-TITULACAO/DOUTORADO") %>>%
        map(~ xml_find_all(., ".//AREAS-DO-CONHECIMENTO")) %>>%
        map(~ xml_children(.)) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> doutorado_area)

    if (nrow(doutorado_area[[1]]) == 0) doutorado_area <- tibble::tibble(nome_grande_area_do_conhecimento = NA, 
                                                                         nome_da_area_do_conhecimento = NA, 
                                                                         nome_da_sub_area_do_conhecimento = NA, 
                                                                         nome_da_especialidade = NA)

    doutorado  %>>%  
        (pmap(list(., doutorado_area), function(x, y) tibble(x, area = list(y)))) %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 

}
