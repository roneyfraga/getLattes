#' @title getLivrosPublicados
#' @description Extract Published Books from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getLivrosPublicados(xmlsLattes[[499]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getLivrosPublicados)
#'  head(bind_rows(lt))
#'  }
#' }
#' @rdname getLivrosPublicados
#' @export 
getLivrosPublicados <- function(curriculo){

  #print(curriculo$id)

  ll <- curriculo$`PRODUCAO-BIBLIOGRAFICA`
  nm <- names(ll)
  encontro <- FALSE

  if(any( nm %in% 'LIVROS-E-CAPITULOS')){
    ll2 <- ll$`LIVROS-E-CAPITULOS`
    nmll2 <- names(ll2)
    if(any( nmll2 %in% 'LIVROS-PUBLICADOS-OU-ORGANIZADOS')){
      ll2 <- ll2$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`
      nmll2 <- names(ll2)
      tnmll2 <- length(nmll2)
      if(tnmll2 > 0){
        testelista <- list()

        ll3 <- lapply(ll2, function(x){

          ll4 <- bind_cols(getCharacter(x$`DADOS-BASICOS-DO-LIVRO`) ,
                           if(any(names(x) %in% 'DETALHAMENTO-DO-LIVRO')){
                             if(length(x$`DETALHAMENTO-DO-LIVRO`) != 0){
                               getCharacter(x$`DETALHAMENTO-DO-LIVRO`)
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
