#' @title getDadosGerais
getDadosGerais <- function(curriculo){
  #print(curriculo$id)
  ll <- curriculo$`DADOS-GERAIS`
  if(any('NOME-COMPLETO' %in% names(ll$.attrs))){
    if(length(ll)>1){
      dados.gerais <- .getCharacter(ll$.attrs)
      dados.gerais$id <- curriculo$id
      dados.gerais$data.atualizacao <- curriculo$.attrs[['DATA-ATUALIZACAO']]
    } else { dados.gerais <- NULL }
  } else { dados.gerais <- NULL }
  return(dados.gerais)
}
