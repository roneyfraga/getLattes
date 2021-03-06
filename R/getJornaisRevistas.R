#' @title getJornaisRevistas
#' @description Extract Newspapers and Magazines from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getJornaisRevistas(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getJornaisRevistas)
#'  head(bind_rows(lt))
#'  }
#' @rdname getJornaisRevistas
#' @export 
getJornaisRevistas <- function(curriculo){

  #print(curriculo$id)

  ll <- curriculo$`PRODUCAO-BIBLIOGRAFICA`
  nm <- names(ll)
  encontro <- FALSE

  if(any( nm %in% 'TEXTOS-EM-JORNAIS-OU-REVISTAS')){
    ll2 <- ll$`TEXTOS-EM-JORNAIS-OU-REVISTAS`
    nmll2 <- names(ll2)
    tnmll2 <- length(nmll2)

    if(tnmll2 > 0){
      testelista <- list()

      ll3 <- lapply(ll2, function(x){

        ll4 <- bind_cols(getCharacter(x$`DADOS-BASICOS-DO-TEXTO`) ,

                         if(any(names(x) %in% 'DETALHAMENTO-DO-TEXTO')){
                           if(length(x$`DETALHAMENTO-DO-TEXTO`) != 0){
                             getCharacter(x$`DETALHAMENTO-DO-TEXTO`)
                           }
                         }

        )

        a <- which(names(x) == "AUTORES" )

        autores <- lapply(a, function(z){ getCharacter(x[[z]])  })

        autores1 <- data.frame(autores = "", autores.citacoes ="", autores.id="")

        for(i in 1:length(autores)){
          if (i == 1){
            autores1$autores <- paste0(autores[[i]]$nome.completo)
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
            autores1$autores <- paste0(autores1$autores, ", " , autores[[i]]$nome.completo)
            autores1$autores.citacoes <- paste0(autores1$autores.citacoes, "/ " , autores[[i]]$nome.para.citacao)
            if (any(names(autores[[i]]) %in% "nro.id.cnpq")){
              if (autores[[i]]$nro.id.cnpq == ""){
                autores1$autores.id <- paste0(autores1$autores.id, ", " , "No.id")
              }else{
                autores1$autores.id <- paste0(autores1$autores.id, ", " , autores[[i]]$nro.id.cnpq)
              }
            }else{
              autores1$autores.id <- paste0(autores1$autores.id, ", " , "No.id")
            }
          }
        }

        id1 <-  getCharacter(curriculo$id)
        names(id1) <- "id"

        ll6 <- bind_cols(ll4,autores1,id1)

      })

      if(length(ll3) > 1 || length(ll3)  == 1 ){
        ll3 <- bind_rows(ll3)
      }

    }

    return(ll3)
  }else{
    ll3 <- NULL
    return(ll3)
  }
}
