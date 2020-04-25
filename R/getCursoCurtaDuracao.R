#' @title getCursoCurtaDuracao
getCursoCurtaDuracao <- function(curriculo){

  #print(curriculo$id)

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

          if(any( names(x) %in% 'DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO')){


            ll4 <- bind_cols(.getCharacter(x$`DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO`) ,

                             if(any(names(x) %in% 'DETALHAMENTO-DE-CURSOS-CURTA-DURACAO-MINISTRADO')){
                               if(length(x$`DETALHAMENTO-DE-CURSOS-CURTA-DURACAO-MINISTRADO`) != 0){
                                 .getCharacter(x$`DETALHAMENTO-DE-CURSOS-CURTA-DURACAO-MINISTRADO`)
                               }
                             }
            )

            a <- which(names(x) == "AUTORES" )

            autores <- lapply(a, function(z){ .getCharacter(x[[z]])  })

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

            id1 <-  .getCharacter(curriculo$id)
            names(id1) <- "id"
            ll6 <- bind_cols(ll4,autores1,id1)

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
