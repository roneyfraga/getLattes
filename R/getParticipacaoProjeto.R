#' @title getParticipacaoProjeto
getParticipacaoProjeto <- function(curriculo){

  #print(curriculo$id)

  ll <- curriculo$`DADOS-GERAIS`
  nm <- names(ll)
  encontro <- FALSE

  if(any( nm %in% 'ATUACOES-PROFISSIONAIS')){

    ll2 <- ll$`ATUACOES-PROFISSIONAIS`
    nmll2 <- names(ll2)
    tnmll2 <- length(nmll2)


    if(tnmll2 > 0 ){

      testelista <- list()
      for(i in 1:tnmll2){
        if(any(names(ll2[[i]]) %in% 'ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO')){

          ll3 <- ll2[[i]]$`ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO`

          tamanholl3 <- length(ll3)

          ll5 <- lapply(ll3, function(x){

            if(any(names(x) %in% 'PROJETO-DE-PESQUISA')){
              if(any(names(x$`PROJETO-DE-PESQUISA`) %in% 'EQUIPE-DO-PROJETO')){

                financiador <- any(names(x$`PROJETO-DE-PESQUISA`) %in% "FINANCIADORES-DO-PROJETO")



                ll4 <- bind_cols(
                  if(financiador){
                    .getCharacter(x$`PROJETO-DE-PESQUISA`$`FINANCIADORES-DO-PROJETO`$`FINANCIADOR-DO-PROJETO`)
                  }else{
                    .getCharacter(list(SEQUENCIA.FINANCIADOR = "",  CODIGO.INSTITUICAO = "", NOME.INSTITUICAO = "" , NATUREZA = "NAO_INFORMADO"))
                  },
                  .getCharacter( x$`PROJETO-DE-PESQUISA`$.attrs),
                  .getCharacter( x$.attrs))

                a <- which(names(x$`PROJETO-DE-PESQUISA`$`EQUIPE-DO-PROJETO`) == "INTEGRANTES-DO-PROJETO" )

                if( !is.null(x$`PROJETO-DE-PESQUISA`$`EQUIPE-DO-PROJETO`)){
                  autores <- lapply(a, function(z){

                    .getCharacter(x$`PROJETO-DE-PESQUISA`$`EQUIPE-DO-PROJETO`[[z]])
                  })

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
                }else{
                  autores1 <- data.frame(autores = "", autores.citacoes ="", autores.id="")
                }


                id1 <-  .getCharacter(curriculo$id)
                names(id1) <- "id"

                ll6 <- bind_cols(ll4,autores1,id1)


              }
            }
          })



          t2 <- bind_rows(ll5)



          encontro <- TRUE
          #print("consegui")

          if(exists("t2")){
            testelista[i] <- list(t2)
          }else{
            testelista[i] <- NULL
          }


        }

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


  }else{
    projetoPesquisa <- NULL
    return(projetoPesquisa)
  }

}
