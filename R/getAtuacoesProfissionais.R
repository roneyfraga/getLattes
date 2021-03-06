#' @title getAtuacoesProfissionais
#' @description Extract profissional links from XML file converted to R list.
#' @param curriculo XML exported from Lattes imported to R as list.
#' @return data frame 
#' @details Curriculum without this information will return NULL. 
#' @examples 
#' if(interactive()){
#'  data(xmlsLattes)
#'  # to import from one curriculum 
#'  getAtuacoesProfissionais(xmlsLattes[[2]])
#'
#'  # to import from two or more curricula
#'  lt <- lapply(xmlsLattes, getAtuacoesProfissionais)
#'  head(bind_rows(lt))
#'  }
#' @rdname getAtuacoesProfissionais
#' @export 
#' @importFrom stringr str_c
getAtuacoesProfissionais <- function(curriculo){
  #print(curriculo$id)
  ll <- curriculo$`DADOS-GERAIS`
  if(any( names(ll) %in% 'ATUACOES-PROFISSIONAIS')){

    ll2 <- ll$`ATUACOES-PROFISSIONAIS`
    qtde <- length(ll2)

    if(qtde!=0){

      if(qtde==1){

        attrs <- if(!any(names(ll2) %in% '.attrs') & any(names(ll2[[1]]) %in% '.attrs')){
          getCharacter(ll2[[1]][['.attrs']])
        } else{
          getCharacter(ll2[[1]])
        }
        atv <- names(ll2[[1]])
        atv <- str_c(atv, collapse=',')
        atv <- gsub(',.attrs', '', atv)
        atv <- tolower(gsub('-','\\.', atv))
        attrs$vinculos <- atv
      }

      if(qtde>1){

        attrs <- lapply(ll2, function(x){
          if(any(names(x) %in% '.attrs')){
            getCharacter(x[['.attrs']])
          } else{
            getCharacter(x)
          }
        })

        attrs <- bind_rows(attrs)
        atv <- lapply(ll2, names)
        atv <- lapply(atv, function(x){ str_c(x, collapse=',')})
        atv <- lapply(atv, function(x){ gsub(',.attrs', '', x) })
        atv <- lapply(atv, function(x){ tolower(gsub('-','\\.', x))})
        atv <- unlist(atv)
        attrs$atividades <- atv
      }
      attrs$id <- curriculo$id
      return(attrs)

    } else{
      attrs <- NULL
      return(attrs)
    }
  } else{
    attrs <- NULL
    return(attrs)
  }
}
