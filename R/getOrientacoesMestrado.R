#' @title getOrientacoesMestrado
getOrientacoesMestrado <- function(curriculo){

  #print(curriculo$id)

  ll <- curriculo$`OUTRA-PRODUCAO`
  nm <- names(ll)
  encontro <- FALSE

  if(any( nm %in% 'ORIENTACOES-CONCLUIDAS')){
    ll2 <- ll$`ORIENTACOES-CONCLUIDAS`
    nmll2 <- names(ll2)
    if(any( nmll2 %in% 'ORIENTACOES-CONCLUIDAS-PARA-MESTRADO')){

      tnmll2 <- length(ll2)
      if(tnmll2 > 0){
        testelista <- list()

        ll3 <- lapply(ll2, function(x){

          if(any( names(x) %in% 'DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO')){

            ll4 <- bind_cols(.getCharacter(x$`DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`),
                             if(any(names(x) %in% 'DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO')){
                               if(length(x$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`) != 0){
                                 .getCharacter(x$`DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO`)
                               }
                             }
            )

            id1 <-  .getCharacter(curriculo$id)
            names(id1) <- "id"
            ll6 <- bind_cols(ll4,id1)

          }
        })

        if(length(ll3) > 1 || length(ll3)  == 1  ){
          ll3 <- bind_rows(ll3)
        }

      }

      return(ll3)

    }else{
      ll3 <- NULL
      return(ll3)
    } #AQUI
  }else{
    ll3 <- NULL
    return(ll3)
  }
}
