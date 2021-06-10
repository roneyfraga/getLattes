#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param curriculo PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[xml2]{xml_attr}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{select}},\code{\link[dplyr]{pull}}
#' @rdname getId
#' @export 
#' @importFrom xml2 xml_attrs
#' @importFrom dplyr bind_rows select pull
#' @importFrom pipeR '%>>%'
getId <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    curriculo %>>%
        xml2::xml_attrs() %>>%
        dplyr::bind_rows() %>>%
        dplyr::mutate(id = as.character(`NUMERO-IDENTIFICADOR`)) %>>%
        dplyr::pull(id)
}
