#' @title getEventosCongressos
#' @description Extract Events and Congresses from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(lattesXML)
#'  # to import from one curriculum 
#'  getEventosCongressos(lattesXML[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(lattesXML, getEventosCongressos)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getEventosCongressos
#' @export 

getEventosCongressos <- function(curriculo){

  #     print(curriculo$id)

  ll <- curriculo$`DADOS-COMPLEMENTARES`
  nm <- names(ll)
  encontro <- FALSE

  if(any( nm %in% 'PARTICIPACAO-EM-EVENTOS-CONGRESSOS')){
    ll2 <- ll$`PARTICIPACAO-EM-EVENTOS-CONGRESSOS`
    nmll2 <- names(ll2)
    if(any( nm %in% 'PARTICIPACAO-EM-EVENTOS-CONGRESSOS' )){

      tnmll2 <- length(ll2)
      if(tnmll2 > 0){

        nomeVariavel <- names(table(unlist(nmll2)))
        llfinal <- list()
        for (y in 1:length(nomeVariavel)){
          g <-  which(names(ll2) == nomeVariavel[y] )

          ll3 <- lapply(g, function(x){

            if(nomeVariavel[y] != 'OUTRAS-PARTICIPACOES-EM-EVENTOS-CONGRESSOS'){
              db <- which(names(ll2[[x]]) == paste0('DADOS-BASICOS-DA-', nomeVariavel[y]) )
              dt <- which(names(ll2[[x]]) == paste0('DETALHAMENTO-DA-', nomeVariavel[y]) )
              partet <-  paste0('DETALHAMENTO-DA-', nomeVariavel[y])
            }else{
              db <- which(names(ll2[[x]]) == paste0('DADOS-BASICOS-DE-', nomeVariavel[y]) )
              dt <- which(names(ll2[[x]]) == paste0('DETALHAMENTO-DE-', nomeVariavel[y]) )
              partet <-  paste0('DETALHAMENTO-DE-', nomeVariavel[y])
            }


            ll4 <- bind_cols( .getCharacter(ll2[[x]][[db]]),
                              if(any( names(ll2[[x]]) %in% partet) ){
                                if(length(ll2[[x]][[dt]]) != 0){
                                  .getCharacter(ll2[[x]][[dt]])
                                }
                              }
            )

            a <- which(names(ll2[[x]]) == "PARTICIPANTE-DE-EVENTOS-CONGRESSOS" )

            autores <- lapply(a, function(z){ .getCharacter(ll2[[x]][[z]])  })

            autores1 <- data.frame(autores = "", autores.citacoes ="", autores.id="")

            for(i in 1:length(autores)){
              if (i == 1){
                autores1$autores <- paste0(autores[[i]]$nome.completo.do.participante.de.eventos.congressos)
                autores1$autores.citacoes<- paste0(autores[[i]]$nome.para.citacao.do.participante.de.eventos.congressos)
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
                autores1$autores <- paste0(autores1$autores, ", " , autores[[i]]$nome.completo.do.participante.de.eventos.congressos)
                autores1$autores.citacoes <- paste0(autores1$autores.citacoes, "/ " , autores[[i]]$nome.para.citacao.do.participante.de.eventos.congressos)
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

            id1 <-  .getCharacter(curriculo$id)
            names(id1) <- "id"
            ll6 <- bind_cols(ll4, autores1, id1)


          })

          if(length(ll3) > 1 || length(g)  == 1 ){
            ll3 <- bind_rows(ll3)
            llfinal[[y]] <- ll3
          }

        } #FIM FOR NOME VARIAVEIS

        llfinal2 <- bind_rows(llfinal)
        return(llfinal2)
      }

      #       llfinal2 <- bind_rows(llfinal)
      #       return(llfinal2)
      #
    }else{
      ll3 <- NULL
      return(ll3)
    } #AQUI
  }else{
    ll3 <- NULL
    return(ll3)
  }
}
