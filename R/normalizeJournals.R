#' @title normalizeJournals
#' @description Normalize journals data: journal name, and ISSN. Two or more journals with the same ISSN will receive the most common Journal Name, then two or more journals with the same name will receive the most common ISSN.
#' @param dataframe data imported with readLattes() then getArtigosPublicados()
#' @param journalName variable with journal name
#' @param issn variable with issn
#' @return data frame
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#' data(latesXML)
#' al <- lapply(xmlsLattes, getArtigosPublicados) 
#' adf <- dplyr::bind_rows(al)
#' 
#' head(
#'     normalizeByDoi(dataframe=adf, 
#'               issn='issn', 
#'               journalName= 'titulo.do.periodico.ou.revista') 
#' ) 
#' 
#'   }
#'  }
#' @seealso 
#'  \code{\link[stringi]{stri_trans_general}}
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{mutate}}
#' @rdname normalizeJournals
#' @export 
#' @importFrom stringi stri_trans_general
#' @importFrom dplyr group_by arrange mutate ungroup mutate_if
#' @importFrom pipeR "%>>%"
normalizeJournals <- function(dataframe, journalName='titulo.do.periodico.ou.revista', issn='issn'){

    a <- dataframe
    a[,'revista'] <- dataframe[,journalName]
    a[,'issn'] <- dataframe[,'issn']

    a %>>% mutate_if(is.factor,as.character) %>>% (. -> a)

    a[,paste0(issn,'_old')] <- a[,issn]
    a[,paste0(journalName,'_old')] <- a[,journalName]


    texto <- a$revista
    texto <- tolower(texto)
    texto <- gsub(" \\(.*\\)", "",texto)
    texto <- gsub(" ,", ",", texto)
    texto <- gsub("  ", " ", texto)
    texto <- gsub('\\s+',' ', texto)
    texto <- gsub("\\.$", "", texto)
    texto <- gsub("^[[:space:]]+|[[:space:]]+$", "", texto)
    texto <- stringi::stri_trans_general(texto, "Latin-ASCII")
    a$revista <- texto

    a %>>%
        dplyr::group_by(issn) %>>%
        dplyr::arrange(revista) %>>%
        dplyr::mutate(revista = mostFrequent(revista)) %>>%
        ungroup() %>>%
        (. -> a)

    a %>>%
        dplyr::group_by(revista) %>>%
        dplyr::arrange(issn) %>>%
        dplyr::mutate(issn = mostFrequent(issn)) %>>%
        ungroup() %>>%
        (. -> a)

    return(a)
}
