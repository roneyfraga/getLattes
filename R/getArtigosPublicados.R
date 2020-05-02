#' @title getArtigosPublicados
#' @description Extract published papers from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getArtigosPublicados(xmlsLattes[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getArtigosPublicados)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getArtigosPublicados
#' @export 
getArtigosPublicados <- function(curriculo){
  #print(curriculo$id)
  lista = curriculo
  nm <- names(lista)
  if(any( nm %in% 'PRODUCAO-BIBLIOGRAFICA')){
    ll2 <- lista$`PRODUCAO-BIBLIOGRAFICA`
    nmll2 <- names(ll2)
    if(any( nmll2 %in% 'ARTIGOS-PUBLICADOS' )){
      ll2 <- ll2$`ARTIGOS-PUBLICADOS`
      nmll2 <- names(ll2)
      tnmll2 <- length(ll2)
      if(tnmll2 > 0){
        nomeVariavel <- names(table(unlist(nmll2)))
        for (y in 1:length(nomeVariavel)){
          g <-  which(names(ll2) == nomeVariavel[y] )

          ll3 <- lapply(g, function(x){

            ll4 <- bind_cols( getCharacter(ll2[[x]]$`DADOS-BASICOS-DO-ARTIGO`),
                              if(any( names(ll2[[x]]) %in% 'DETALHAMENTO-DO-ARTIGO') ){
                                if(length(ll2[[x]]$`DETALHAMENTO-DO-ARTIGO`) != 0){
                                  getCharacter(ll2[[x]]$`DETALHAMENTO-DO-ARTIGO`)
                                }
                              }
            )

            a <- which(names(ll2[[x]]) == "AUTORES" )

            autores <- lapply(a, function(z){ getCharacter(ll2[[x]][[z]])  })

            autores1 <- data.frame(autores = "", autores.citacoes ="", autores.id="")

            for(i in 1:length(autores)){
              if (i == 1){
                autores1$autores <- paste0(autores[[i]]$nome.completo.do.autor)
                autores1$autores.citacoes<- paste0(autores[[i]]$nome.para.citacao)
                if (any(names(autores[[i]]) %in% "nro.id.cnpq")){
                  if (autores[[i]]$nro.id.cnpq == ""){
                    autores1$autores.id <- paste0("No.id")
                  }else{
                    autores1$autores.id <- paste0(autores[[i]]$nro.id.cnpq)
                  }
                }else{
                  autores1$autores.id <- paste0("No.id")
                }
              }else{
                autores1$autores <- paste0(autores1$autores, "; " , autores[[i]]$nome.completo.do.autor)
                autores1$autores.citacoes <- paste0(autores1$autores.citacoes, "/ " , autores[[i]]$nome.para.citacao)
                if (any(names(autores[[i]]) %in% "nro.id.cnpq")){
                  if (autores[[i]]$nro.id.cnpq == ""){
                    autores1$autores.id <- paste0(autores1$autores.id, "; " , "No.id")
                  }else{
                    autores1$autores.id <- paste0(autores1$autores.id, "; " , autores[[i]]$nro.id.cnpq)
                  }
                }else{
                  autores1$autores.id <- paste0(autores1$autores.id, "; " , "No.id")
                }
              }
            }

            id1 <-  getCharacter(curriculo$id)
            names(id1) <- "id"

            ll6 <- bind_cols(ll4, autores1, id1)

          })

          if(length(ll3) > 1 || length(g)  == 1 ){
            ll3 <- bind_rows(ll3)
          }

        } #FIM FOR NOME VARIAVEIS
        return(ll3)
      }else{
        ll3 <- NULL
        return(ll3)
      }
    }else{
      ll3 <- NULL
      return(ll3)
    } #AQUI
  }else{
    ll3 <- NULL
    return(ll3)
  }
}
