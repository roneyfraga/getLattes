#' Title
#'
#' @param dataframe
#' @param journalName
#' @param issn
#'
#' @return
#' @export
#'
#' @examples
normalizeJournals <- function(dataframe, journalName='revista', issn='issn'){

  # recebe data frame com título da revista e issn

  a <- dataframe
  a[,'revista'] <- dataframe[,journalName]
  a[,'issn'] <- dataframe[,'issn']

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

  a %>%
    dplyr::group_by(issn) %>%
    dplyr::arrange(revista) %>%
    dplyr::mutate(revista = mostFrequent(revista)) %>>%
    ungroup() %>>%
    (. -> a)

  a %>%
    dplyr::group_by(revista) %>%
    dplyr::arrange(issn) %>%
    dplyr::mutate(issn = mostFrequent(issn)) %>>%
    ungroup() %>>%
    (. -> a)

  a
}
