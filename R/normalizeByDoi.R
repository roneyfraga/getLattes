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
#' al <- lapply(lattesXML, getArtigosPublicados) 
#' adf <- bind_rows(al)
#' 
#' head(
#'     normalizeByDoi(dataframe=adf, 
#'               doi='doi', 
#'               year='ano.do.artigo', 
#'               issn='issn', 
#'               paperTitle='titulo.do.artigo', 
#'               journalName= 'titulo.do.periodico.ou.revista') 
#' 
#'   }
#'  }
#' @seealso 
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{mutate}}
#' @rdname normalizeByDoi
#' @export 
#' @importFrom dplyr group_by arrange mutate
#' @importFrom pipeR "%>>%"
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
normalizeByDoi <- function(dataframe,doi='doi',year='ano',issn='issn',paperTitle='titulo',journalName='revista'){

  data.frame(doi=dataframe[,'doi']) %>>% 
      as_tibble %>>% 
      mutate(doi=as.character(doi)) %>>% 
      (. -> a)

  a[,'revista'] <- as.character(dataframe[,journalName])
  a[,'issn'] <- as.character(dataframe[,issn])
  a[,'ano'] <- as.character(dataframe[,year])
  a[,'titulo'] <- as.character(dataframe[,paperTitle])

  # no[DOI,Revista,ISSN,Ano]
  a[ is.na(a$doi) | a$doi=='' , 'doi'] <- paste0('noDOI_',1:nrow(a[ is.na(a$doi) | a$doi=='', 'doi']))
  a[ is.na(a$revista) | a$revista=='' , 'revista'] <- paste0('noRevsita_',1:nrow(a[ is.na(a$revista) | a$revista=='', 'revista']))
  a[ is.na(a$issn) | a$issn=='' , 'issn'] <- paste0('noISSN_',1:nrow(a[ is.na(a$issn) | a$issn=='', 'issn']))
  a[ is.na(a$ano) | a$ano=='' , 'issn'] <- paste0('noAno_',1:nrow(a[ is.na(a$ano) | a$ano=='', 'ano']))

  a %>>%
    dplyr::group_by(!! doi) %>>%
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

  return(a)
}
