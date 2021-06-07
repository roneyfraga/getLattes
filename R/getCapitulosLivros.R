#' @title getCapitulosLivros
#' @description Extract Books Chapter from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getCapitulosLivros(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getCapitulosLivros)
#'  head(bind_rows(lt))
#'  }
#' @rdname getCapitulosLivros
#' @export 
getCapitulosLivros <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//CAPITULOS-DE-LIVROS-PUBLICADOS/CAPITULO-DE-LIVRO-PUBLICADO") %>>%
        map(~ xml_find_all(., ".//DADOS-BASICOS-DO-CAPITULO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> dados_basicos)

    xml_find_all(curriculo, ".//CAPITULOS-DE-LIVROS-PUBLICADOS/CAPITULO-DE-LIVRO-PUBLICADO") %>>%
        map(~ xml_find_all(., ".//DETALHAMENTO-DO-CAPITULO")) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        (. -> detalhamento)

    xml_find_all(curriculo, ".//CAPITULOS-DE-LIVROS-PUBLICADOS/CAPITULO-DE-LIVRO-PUBLICADO") %>>%
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
