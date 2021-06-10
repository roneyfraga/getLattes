#' @title getId
#' @description Extract the unique 16 digits identification from 'Lattes' XML file.
#' @param curriculo 'Lattes' XML imported as `xml2::read_xml()`.
#' @return character
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()) {
#'  
#'  # to import from one curriculum 
#'  # curriculo <- xml2::read_xml('file.xml')
#'  # getId(curriculo)
#'
#'  }
#' @seealso 
#'  \code{\link[xml2]{xml_attr}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{select}},\code{\link[dplyr]{pull}}
#' @rdname getId
#' @export 
#' @importFrom xml2 xml_attrs
#' @importFrom dplyr bind_rows select pull
getId <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
        stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    att <- 
        xml2::xml_attrs(curriculo) |>
        dplyr::bind_rows() 

        as.character(att$`NUMERO-IDENTIFICADOR`)
}
