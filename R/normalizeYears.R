#' @title normalizeYears
#' @description FUNCTION_DESCRIPTION
#' @param dataframe data imported with readLattes() then getArtigosPublicados()
#' @param year2normalize variable year 
#' @param issn variable with issn, Default: 'issn'
#' @param journalName variable with journal name, Default: 'revista'
#' @param paperTitle variable with paper title, Default: 'titulo'
#' @return data frame
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#' data(latesXML)
#' l <- lapply(xmlsLattes, getArtigosPublicados) 
#' df <- bind_rows(al) %>>% tbl_df
#' 
#' ead(
#'     normalizeYears(dataframe=adf, 
#'               year2normalize='ano.do.artigo',
#'               issn='issn', 
#'               journalName= 'titulo.do.periodico.ou.revista',
#'               paperTitle='titulo.do.artigo') 
#' ) 
#' 
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{mutate}}
#' @rdname normalizeYears
#' @export 
#' @importFrom dplyr group_by arrange mutate
#' @importFrom pipeR "%>>%"
normalizeYears <- function(dataframe,year2normalize='ano.do.artigo',issn='issn',journalName='titulo.do.periodico.ou.revista',paperTitle='titulo.do.artigo'){

    titulo <- revista <- ano_old <- NULL

    a <- dataframe
    a[,'ano'] <- dataframe[,year2normalize]
    a[,'titulo'] <- dataframe[,paperTitle]
    a[,'revista'] <- dataframe[,journalName]
    a[,'ano_old'] <- dataframe[,year2normalize]

    a %>>% mutate_if(is.factor,as.character) %>>% (. -> a)

    a  %>>%
        dplyr::group_by(titulo,revista,issn) %>>%
        dplyr::arrange(ano_old) %>>%
        dplyr::mutate(ano = mostFrequent(ano_old)) %>>%
        ungroup() %>>%
        (. -> a)

    return(a)
}
