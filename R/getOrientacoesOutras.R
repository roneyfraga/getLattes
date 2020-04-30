#' @title getOrientacoesOutras
#' @description Extract Academic Advisory (Other) from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(lattesXML)
#'  # to import from one curriculum 
#'  getOrientacoesOutras(lattesXML[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(lattesXML, getOrientacoesOutras)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getOrientacoesOutras
#' @export 
getOrientacoesOutras <- function(curriculo){

  #print(curriculo$id)

  ll <- curriculo$`OUTRA-PRODUCAO`
  nm <- names(ll)
  encontro <- FALSE

  if(any( nm %in% 'ORIENTACOES-CONCLUIDAS')){
    ll2 <- ll$`ORIENTACOES-CONCLUIDAS`
    nmll2 <- names(ll2)
    if(any( nmll2 %in% 'OUTRAS-ORIENTACOES-CONCLUIDAS')){

      tnmll2 <- length(ll2)
      if(tnmll2 > 0){
        testelista <- list()

        ll3 <- lapply(ll2, function(x){

          if(any( names(x) %in% 'DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS')){

            ll4 <- bind_cols(.getCharacter(x$`DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS`),
                             if(any(names(x) %in% 'DETALHAMENTO-DE-OUTRAS-ORIENTACOES-CONCLUIDAS')){
                               if(length(x$`DETALHAMENTO-DE-OUTRAS-ORIENTACOES-CONCLUIDAS`) != 0){
                                 .getCharacter(x$`DETALHAMENTO-DE-OUTRAS-ORIENTACOES-CONCLUIDAS`)
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
