#' @title getId
#' @description 
#' @param 
#' @return 
#' @details 
#' @examples 
#' 
#'  
#' @rdname getId
#' @export 
#' @importFrom pipeR "%>>%"
getId <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    curriculo %>>%
        xml2::xml_attrs() %>>%
        dplyr::bind_rows() %>>%
        dplyr::select(id = `NUMERO-IDENTIFICADOR`) %>>%
        dplyr::pull(id)
}
