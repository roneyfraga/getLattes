#' Title
#'
#' @param dataframe
#' @param year2normalize
#' @param issn
#' @param journalName
#' @param paperTitle
#'
#' @return
#' @export
#'
#' @examples
normalizeYears <- function(dataframe,year2normalize='ano',issn='issn',journalName='revista',paperTitle='titulo'){

  # recebe data frame com título do artigo e issn,
  # já normalizados, e checa anomalias no ano do artigo

  a <- dataframe
  a[,'ano'] <- dataframe[,year2normalize]
  a[,'titulo'] <- dataframe[,paperTitle]
  a[,'revista'] <- dataframe[,journalName]
  a[,'ano_old'] <- dataframe[,year2normalize]

  a  %>%
    dplyr::group_by(titulo,revista,issn) %>%
    dplyr::arrange(ano_old) %>%
    dplyr::mutate(ano = mostFrequent(ano_old)) %>>%
    ungroup() %>>%
    (. -> a)
  a
}
