#------------------------------
# Function to extract and format names

#' @title .vname
.vname <- function(x){ tolower(gsub('-','\\.', names(x)))}

#------------------------------
# função para dividir um vetor em partes de X elementos

#' @title chunks
chunks <- function(lista, tamanho.grupo){
  chunks <- split(lista, ceiling(seq_along(lista)/tamanho.grupo))
  names(chunks) <- NULL
  return(chunks)
}

#------------------------------
# função para selecionar determinadas colunas, apenas quanto existe todas as variáveis

#' @title ssubset
ssubset <- function(x, nomes.variaveis){
  if(all( nomes.variaveis %in% names(x)  )==TRUE){
    if(length(x)>=1){
      x =  subset(x, select=nomes.variaveis)
    }
  } else { x <- NULL}
  return(x)
}

#------------------------------
# função para transformar character em data.frame

#' @title .getCharacter
.getCharacter <- function(x){
  x %>>%
    as.list(.) %>>%
    (do.call(rbind, .)) %>>%
    (data.frame(t(.))) -> df

  names(df) = tolower(names(df))
  return(df)
}

#------------------------------
# função para retornar apenas data.frames a partir de uma lista

#' @title only.list.of.df
only.list.of.df <- function(x){
  x %>>%
    ( ~ temp ) %>>%
    (lapply(., function(x){ is.data.frame(x)  }))  %>>%
    ( unlist(.) ) %>>%
    ( temp[.] )  %>>%
    ( ~ resultado )
  return(resultado)
}

#------------------------------
# função para o total de vezes que determinada variável aparece

#' @title getVariableOcc
getVariableOcc <- function(x){
  x %>>%
    (lapply(.,names)) %>>%
    (~ cat("Total de curriculos:",length(.),"\n")) %>>%
    (unlist(.)) %>>%
    (as.data.frame(cbind(table(.)))) %>>%
    (mutate(.,Variavel=row.names(.),Ocorrencias=V1)) %>>%
    (select(.,Variavel,Ocorrencias)) %>>%
    (arrange(., desc(Ocorrencias)))
}

#------------------------------
# função padronizar texto

#' @title clean.text
clean.text <- function(texto=texto){
  texto <- gsub("^[[:space:]]+|[[:space:]]+$", "", as.character(texto))
  texto <- tolower(texto)
  texto <- stri_trans_general(texto, "Latin-ASCII")
  return(as.character(texto))
}

#------------------------------
# http://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
# Ferdinand.kraft

#' @title expand.grid.unique
expand.grid.unique <- function(x, y, include.equals=FALSE)
{
  x <- unique(x)
  y <- unique(y)
  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  as.data.frame(do.call(rbind, lapply(seq_along(x), g)))
}

#------------------------------
# padronizar dados obtidos em getFormacao()

#' @title formacao2df
formacao2df <- function(lt){
  for(i in seq_along(lt)){
    if(length(lt[[i]])>1){
      for(j in 1:(length(lt[[i]])-1)){
        if(any(names(lt[[i]][[j]]) %in% '.attrs')){
          if(is.factor(lt[[i]][[j]]$.attrs)){
            lt[[i]][[j]] <- as.data.frame(t(lt[[i]][[j]]))
            names(lt[[i]][[j]]) %>>% (tolower(.)) %>>% (gsub("-", ".",.))
          }
          if(is.list(lt[[i]][[j]]$.attrs)){
            lt[[i]][[j]]  <- .getCharacter(lt[[i]][[j]]$.attrs)
          }
        }
        lt[[i]][[j]]$id <- lt[[i]]$id
      }
      lt[[i]]$id <- NULL
    }
  }
  return(lt)
}

#' @title mostFrequent
mostFrequent <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

