#' @title getApresentacaoTrabalho
#' @description Extract academic's papers presentation, from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getApresentacaoTrabalho(xmlsLattes[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getApresentacaoTrabalho)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getApresentacaoTrabalho
#' @export 
#' @importFrom dplyr bind_cols bind_rows 
getApresentacaoTrabalho <- function(curriculo){

    #print(curriculo$id)

    if(is.na(curriculo)){ stop("Select a input file.")}
    if(!is.list(curriculo)){ stop("Input file is not a curriculum list.")}

    ll <- curriculo$`PRODUCAO-TECNICA`
    nm <- names(ll)
    encontro <- FALSE

    if(any( nm %in% 'TRABALHO-TECNICO')){
        ll2 <- ll
        nmll2 <- names(ll2)
        if(any( nmll2 %in% 'DEMAIS-TIPOS-DE-PRODUCAO-TECNICA')){
            ll2 <- ll2$`DEMAIS-TIPOS-DE-PRODUCAO-TECNICA`
            tnmll2 <- length(ll2)
            if(tnmll2 > 0){
                testelista <- list()

                ll3 <- lapply(ll2, function(x){

                                  if(any( names(x) %in% 'DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO')){


                                      ll4 <- bind_cols(getCharacter(x$`DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO`) ,

                                                       if(any(names(x) %in% 'DETALHAMENTO-DA-APRESENTACAO-DE-TRABALHO')){
                                                           if(length(x$`DETALHAMENTO-DA-APRESENTACAO-DE-TRABALHO`) != 0){
                                                               getCharacter(x$`DETALHAMENTO-DA-APRESENTACAO-DE-TRABALHO`)
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

}})

                if(length(ll3) > 1 || length(ll3)  == 1  ){
                    ll3 <- bind_rows(ll3)
                }

            }
            return(ll3)

        }else{
            ll3 <- NULL
            return(ll3)
        } 
    }else{
        ll3 <- NULL
        return(ll3)
    }
}
