#' @title normalizeByDoi
#' @description Normalize articles data: title, journal name, ISSN, and year by DOI code. The most common information (title, journal name, ISSN, and year) will be inserted into all articles with the same DOI code.
#' @param dataframe data imported with readLattes() then getArtigosPublicados()
#' @param doi variable with DOI code
#' @param year variable with year
#' @param issn variable with issn
#' @param paperTitle variable with paper title
#' @param journalName variable with journal name
#' @return data frame
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#' data(latesXML)
#' al <- lapply(xmlsLattes, getArtigosPublicados) 
#' adf <- bind_rows(al)
#' 
#' head(
#'     normalizeByDoi(dataframe=adf, 
#'               doi='doi', 
#'               year='ano.do.artigo', 
#'               issn='issn', 
#'               paperTitle='titulo.do.artigo', 
#'               journalName= 'titulo.do.periodico.ou.revista')
#' ) 
#' 
#'   }
#'  }
#' @seealso 
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{mutate}}
#' @rdname normalizeByDoi
#' @export 
#' @importFrom dplyr group_by arrange mutate mutate_if rename
#' @importFrom pipeR "%>>%"
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
normalizeByDoi <- function(dataframe,doi='doi',year='ano.do.artigo',issn='issn',paperTitle='titulo.do.artigo',journalName='titulo.do.periodico.ou.revista'){

    dataframe$key <- 1:nrow(dataframe)

    data.frame(key=dataframe[,'key']) %>>% (as_tibble(.) -> a)

    a[,'doi'] <- dataframe[,doi]
    a[,'revista'] <- dataframe[,journalName]
    a[,'issn'] <- dataframe[,issn]
    a[,'ano'] <- dataframe[,year]
    a[,'titulo'] <- dataframe[,paperTitle]

    a %>>% mutate_if(is.factor,as.character) %>>% (. -> a)

    # no[DOI,Revista,ISSN,Ano]
    a[ is.na(a$doi) | a$doi=='' , 'doi'] <- paste0('noDOI_',1:nrow(a[ is.na(a$doi) | a$doi=='', 'doi']))
    a[ is.na(a$revista) | a$revista=='' , 'revista'] <- paste0('noRevsita_',1:nrow(a[ is.na(a$revista) | a$revista=='', 'revista']))
    a[ is.na(a$issn) | a$issn=='' , 'issn'] <- paste0('noISSN_',1:nrow(a[ is.na(a$issn) | a$issn=='', 'issn']))
    a[ is.na(a$ano) | a$ano=='' , 'issn'] <- paste0('noAno_',1:nrow(a[ is.na(a$ano) | a$ano=='', 'ano']))

    a %>>%
        dplyr::group_by(doi) %>>%
        dplyr::arrange(titulo) %>>%
        dplyr::mutate(titulo = mostFrequent(titulo)) %>>%
        ungroup() %>>%
        (. -> a)

    a %>>%
        dplyr::group_by(doi) %>>%
        dplyr::arrange(revista) %>>%
        dplyr::mutate(revista = mostFrequent(revista)) %>>%
        ungroup() %>>%
        (. -> a)

    a %>>%
        dplyr::group_by(doi) %>>%
        dplyr::arrange(issn) %>>%
        dplyr::mutate(issn = mostFrequent(issn)) %>>%
        ungroup() %>>%
        (. -> a)

    a %>>%
        dplyr::group_by(doi) %>>%
        dplyr::arrange(ano) %>>%
        dplyr::mutate(ano = mostFrequent(ano)) %>>%
        ungroup() %>>%
        (. -> a)

    a %>>% 
        dplyr::rename( !! year := ano)  %>>%
        dplyr::rename( !! journalName := revista ) %>>% 
        dplyr::rename( !! paperTitle := titulo ) %>>% 
        (. -> a)

    dataframe %>>% 
        dplyr::rename( !! paste0(paperTitle,'_old') := !! paperTitle) %>>% 
        dplyr::rename( !! paste0(journalName,'_old') := !! journalName) %>>% 
        dplyr::rename( !! paste0(year,'_old') := !! year) %>>% 
        dplyr::rename( !! paste0(issn,'_old') := !! issn) %>>% 
        left_join(a, by='key') %>>% 
        select(-key) %>>% 
        (return(.))

}
