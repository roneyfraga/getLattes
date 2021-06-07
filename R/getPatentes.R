
getPatentes <- function(curriculo) {

    if (!any(class(curriculo) == 'xml_document')) {
            stop("The input file must be XML, imported from `xml2` package.", call. = FALSE)
    }

    xml_find_all(curriculo, ".//DADOS-BASICOS-DA-PATENTE") %>>%
        xml_attrs() %>>%
        bind_rows() %>>%
        (janitor::clean_names(.)) %>>%
        dplyr::distinct(.keep_all = TRUE) %>>%
        dplyr::mutate(id = getId(curriculo))
}

