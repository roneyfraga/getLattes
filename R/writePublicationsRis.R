
#' @title writePublicationsRis
#' @description Function to convert and export Published Articles and Books to RIS format.
#' @param x data frame 
#' @param filename external file where the data will be saved, Default: 'papers.ris'
#' @param append to an existing file, Default: F
#' @param citationName authors name field, Default: F
#' @param tableLattes bibliographic data to be exported, 'ArtigosPublicados', 'Livros' or 'CapitulosLivros', Default: 'ArtigosPublicados'
#' @return data frame
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname writePublicationsRis
#' @export 
writePublicationsRis <- function(x, filename = 'papers.ris', append = F, citationName = F, tableLattes = 'ArtigosPublicados') {

  filename2 <- strsplit(filename, split = '\\.') 
  filename_ext <- filename2[[1]][length(filename2[[1]])]

  if (filename_ext != 'ris') {
    stop("The file extention must be '.ris'", call. = FALSE)
  }

  if (!is.logical(append)) {
    stop("'append' must be logical (TRUE or FALSE)", call. = FALSE)
  }

  if (!is.logical(citationName)) {
    stop("'citationName' must be logical (TRUE or FALSE)", call. = FALSE)
  }

  if (!any(tableLattes == c('ArtigosPublicados', 'Livros', 'CapitulosLivros'))) {
    stop("'tableLattes' must be a getLattes data.frame, as: 'ArtigosPublicados', 'Livros', or 'CapitulosLivros'", call. = FALSE)
  }

  if (append != T) write("", file = filename, append = F)

  if (tableLattes == 'ArtigosPublicados') {

    # cat('lendo artigos', '\n')

    for (i in 1:nrow(x)) {
      write("TY  - JOUR", file = filename, append = T)
      write(paste0("TI  - ", x$titulo_do_artigo[i]), file = filename, append = T)
      write(paste0("PY  - ", x$ano_do_artigo[i]), file = filename, append = T)
      write(paste0("T2  - ", x$titulo_do_periodico_ou_revista[i]), file = filename, append = T)
      write(paste0("SN  - ", x$issn[i]), file = filename, append = T)

      if (citationName == F) { 
        for (k in seq_along(x$autores[[i]]$nome_completo_do_autor)) { 
          write(paste0("AU  - ", x$autores[[i]]$nome_completo_do_autor[k]), file = filename, append = T)
        }
      } else { 
        for (k in seq_along(x$autores[[i]]$nome_para_citacao)) { 
          write(paste0("AU  - ", strsplit(as.character(x$autores[[i]][k, 'nome_para_citacao']), split = ';')[[1]][1]), file = filename, append = T)
        }
      }

      write(paste0("VL  - ", x$volume[i]), file = filename, append = T)
      write(paste0("IS  - ", x$serie[i]), file = filename, append = T)
      write(paste0("DO  - ", x$doi[i]), file = filename, append = T)
      write(paste0("SP  - ", x$pagina_inicial[i]), file = filename, append = T)
      write(paste0("EP  - ", x$pagina_final[i]), file = filename, append = T)
      write(paste0("UR  - ", x$home_page_do_trabalho[i]), file = filename, append = T)
      write(paste0("LA  - ", x$idioma[i]), file = filename, append = T)
      write("ER  - \n", file = filename, append = T)

    }
  }

  if (tableLattes == 'Livros') {

    # cat('lendo livros', '\n')

    for (i in 1:nrow(x)) {

      write("TY  - BOOK", file = filename, append = T)
      write(paste0("TI  - ", x$titulo_do_livro[i]), file = filename, append = T)
      write(paste0("T2  - ", x$titulo_do_livro_ingles[i]), file = filename, append = T)
      write(paste0("PY  - ", x$ano[i]), file = filename, append = T)

      if (citationName == F) { 
        for (k in seq_along(x$autores[[i]]$nome_completo_do_autor)) { 
          write(paste0("AU  - ", x$autores[[i]]$nome_completo_do_autor[k]), file = filename, append = T)}
      } else { 
        for (k in seq_along(x$autores[[i]]$nome_para_citacao)) { 
          write(paste0("AU  - ", strsplit(as.character(x$autores[[i]][k, 'nome_para_citacao']), split = ';')[[1]][1]), file = filename, append = T)
        }
      }

      write(paste0("ED  - ", x$nome_da_editora[i]), file = filename, append = T)
      write(paste0("DO  - ", x$doi[i]), file = filename, append = T)
      write(paste0("LA  - ", x$idioma[i]), file = filename, append = T)
      write(paste0("SN  - ", x$isbn[i]), file = filename, append = T)
      write(paste0("SER  - ", x$numero_da_serie[i]), file = filename, append = T)
      write(paste0("PB  - ", x$nome_da_editora[i]), file = filename, append = T)
      write(paste0("AD  - ", x$cidade_da_editora[i]), file = filename, append = T)
      write("ER  - \n", file = filename, append = T)
    }
  } 

  if (tableLattes == 'CapitulosLivros') {

    # cat('lendo capitulos de livros', '\n')

    for (i in 1:nrow(x)) {

      write("TY  - SER", file = filename, append = T)
      write(paste0("TI  - ", x$titulo_do_capitulo_do_livro[i]), file = filename, append = T)
      write(paste0("T2  - ", x$titulo_do_livro[i]), file = filename, append = T)
      write(paste0("PY  - ", x$ano[i]), file = filename, append = T)

      if (citationName == F) { 
        for (k in seq_along(x$autores[[i]]$nome_completo_do_autor)) { write(paste0("AU  - ", x$autores[[i]]$nome_completo_do_autor[k]), file = filename, append = T)}
      } else { 
        for (k in seq_along(x$autores[[i]]$nome_para_citacao)) { 
          write(paste0("AU  - ", strsplit(as.character(x$autores[[i]][k, 'nome_para_citacao']), split = ';')[[1]][1]), file = filename, append = T) 
        }
      }

      write(paste0("ED  - ", x$nome_da_editora[i]), file = filename, append = T)
      write(paste0("DO  - ", x$doi[i]), file = filename, append = T)
      write(paste0("LA  - ", x$idioma[i]), file = filename, append = T)
      write(paste0("SN  - ", x$isbn[i]), file = filename, append = T)
      write(paste0("SER  - ", x$numero_da_serie[i]), file = filename, append = T)
      write(paste0("PB  - ", x$nome_da_editora[i]), file = filename, append = T)
      write(paste0("AD  - ", x$cidade_da_editora[i]), file = filename, append = T)
      write(paste0("SP  - ", x$pagina_inicial[i]), file = filename, append = T)
      write(paste0("EP  - ", x$pagina_final[i]), file = filename, append = T)
      write("ER  - \n", file = filename, append = T)

    }
  }
}

