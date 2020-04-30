#' @title getLinhaPesquisa
#' @description Extract Research Lines from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(lattesXML)
#'  # to import from one curriculum 
#'  getLinhaPesquisa(lattesXML[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(lattesXML, getLinhaPesquisa)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getLinhaPesquisa
#' @export 
getLinhaPesquisa <- function(curriculo){
  #print(curriculo$id)

  lista = curriculo

  ll <- lista$`DADOS-GERAIS`
  nm <- names(ll)
  encontro <- FALSE
  if(any( nm %in% 'ATUACOES-PROFISSIONAIS')){

    ll2 <- ll$`ATUACOES-PROFISSIONAIS`
    nmll2 <- names(ll2)
    tnmll2 <- length(nmll2)

    if(tnmll2 > 0 ){

      testelista <- list()
      for(i in 1:tnmll2){

        if(any(names(ll2[[i]]) %in% 'ATIVIDADES-DE-PESQUISA-E-DESENVOLVIMENTO')){

          ll3  <- ll2[[i]]$`ATIVIDADES-DE-PESQUISA-E-DESENVOLVIMENTO`
          nmpd <- length(names(ll3))


          if(nmpd != 0){



            t1 <- lapply(ll3, function(x){{

              a <- which(names(x) == "LINHA-DE-PESQUISA" )
              att <- which(names(x) == ".attrs")

              b <- lapply(a, function(z){


                #.getCharacter(x[[z]])
                if(any(names(x[[z]]) %in% 'PALAVRAS-CHAVE' | names(x[[z]])   %in% 'AREAS-DO-CONHECIMENTO'
                       | names(x[[z]])   %in% 'SETORES-DE-ATIVIDADE'
                )){
                  .getCharacter(x[[z]]$.attrs)
                }else{
                  .getCharacter(x[[z]])
                }

              })

              c <- lapply(b, function(z){ bind_cols(z,.getCharacter(x$.attrs),
                                                    .getCharacter( list (id = curriculo$id ))
              )


              })

              d <- bind_rows(c)




            }})

            t2 <- bind_rows(t1)



            encontro <- TRUE
            # print("consegui")

            if(exists("t2")){
              testelista[i] <- list(t2)
            }else{
              testelista[i] <- NULL
            }

          }

          # print(i)
        }

      }

      if(encontro){
        x <- testelista
        a <-  Filter(Negate(is.null), x)

        if(length(a) > 1 ){
          linhaPesquisa <- list(bind_rows(a))
        }else{
          linhaPesquisa <- a
        }
        return(linhaPesquisa)
        encontro <- FALSE
      }else{
        linhaPesquisa <- NULL
        return(linhaPesquisa)
        encontro <- FALSE
      }

    }
  }else{
    linhaPesquisa <- NULL
    return(linhaPesquisa)
  }
}
