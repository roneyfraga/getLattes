#' @title getIdiomas
#' @description Extract Languages from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getIdiomas(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getIdiomas)
#'  head(bind_rows(lt))
#'  }
#' @rdname getIdiomas
#' @export 
getIdiomas <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//IDIOMAS") %>>%
        map(~ xml_children(.)) %>>%
        map(~ xml_attrs(.)) %>>%
        map(~ bind_rows(.)) %>>%
        map(~ janitor::clean_names(.)) %>>%
        bind_rows() %>>%
        dplyr::mutate(id = getId(curriculo)) 
        
}
