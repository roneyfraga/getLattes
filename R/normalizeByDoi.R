normalizeByDoi <- function(dataframe,doi='doi',year='ano',issn='issn',paperTitle='titulo',journalName='revista'){

  a <- dataframe

  a[,'revista'] <- dataframe[,journalName]
  a[,'issn'] <- dataframe[,issn]
  a[,'ano'] <- dataframe[,year]
  a[,'titulo'] <- dataframe[,paperTitle]

  # no[DOI,Revista,ISSN,Ano]
  a[ is.na(a$doi) | a$doi=='' , 'doi'] <- paste0('noDOI_',1:nrow(a[ is.na(a$doi) | a$doi=='', 'doi']))
  a[ is.na(a$revista) | a$revista=='' , 'revista'] <- paste0('noRevsita_',1:nrow(a[ is.na(a$revista) | a$revista=='', 'revista']))
  a[ is.na(a$issn) | a$issn=='' , 'issn'] <- paste0('noISSN_',1:nrow(a[ is.na(a$issn) | a$issn=='', 'issn']))
  a[ is.na(a$ano) | a$ano=='' , 'issn'] <- paste0('noAno_',1:nrow(a[ is.na(a$ano) | a$ano=='', 'ano']))

  a %>%
    dplyr::group_by(doi) %>%
    dplyr::arrange(titulo) %>%
    dplyr::mutate(titulo = mostFrequent(titulo)) %>>%
    ungroup() %>>%
    (. -> a)

  a %>%
    dplyr::group_by(doi) %>%
    dplyr::arrange(revista) %>%
    dplyr::mutate(revista = mostFrequent(revista)) %>>%
    ungroup() %>>%
    (. -> a)

  a %>%
    dplyr::group_by(doi) %>%
    dplyr::arrange(issn) %>%
    dplyr::mutate(issn = mostFrequent(issn)) %>>%
    ungroup() %>>%
    (. -> a)

  a %>%
    dplyr::group_by(doi) %>%
    dplyr::arrange(ano) %>%
    dplyr::mutate(ano = mostFrequent(ano)) %>>%
    ungroup() %>>%
    (. -> a)
  a

}
