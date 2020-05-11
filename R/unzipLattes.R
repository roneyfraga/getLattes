#' @title unzipLattes
#' @description Unzip Lattes file to XML preserving 16 digits id as the file name.
#' @param filezip Zip files in your local machine.
#' @param path Directory with zip files.
#' @details Only zip files with 16 digits Lattes ids will be extracted.  
#' @examples 
#' \dontrun{
#' if(interactive()){
#' 
#' unzipLattes(filezip='*.zip')
#'  
#' # to import from a diferent directory
#' # unzipLattes(filezip='*.zip', path='data/')
#'  }
#' }
#' @export 
#' @importFrom utils unzip
unzipLattes <- function(filezip='*.zip$', path='.'){

    if(path!='.'){ 
        path_old <- getwd()
        setwd(path) 
    }

    files <- list.files(pattern=filezip)

    # keep only lattes 16 digits ids zip files
    lapply(files, function(x){ nchar( as.character(strsplit(x,'\\.')[[1]][1])) ==16 }) %>>% 
        unlist() %>>% 
        (. -> isLattes)

    lapply(files[isLattes], function(x){ 
               unzip(x) 
               file.rename(from='curriculo.xml', to= paste0(as.character(strsplit(x,'\\.')[[1]][1]),'.xml')) }
    )

    if(path!='.'){ setwd(path_old) }
}
