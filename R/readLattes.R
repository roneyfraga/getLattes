#' @title readLattes
#' @description Import Lattes exported XML to R list.
#' @param filexml XML file exported from Lattes, can be a pattern '*.xml$'.
#' @param path Directory with xml files.
#' @return list
#' @details filexml XML file locally stored 
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # to import only one file
#' readLattes(filexml='4984859173592703.zip.xml')
#' # to import several files
#' readLattes(filexml='*.xml$'))
#' }
#' @rdname readLattes
#' @export 
#' @importFrom XML xmlParse xmlToList
readLattes <- function(filexml='.xml$', path='.'){

    if(path!='.'){ 
        path_old <- getwd()
        setwd(path) 
    }

    files <- list.files(pattern=filexml)

    curriculo <- vector('list', length(files))

    if( length(files)==1){
        xmltoparse <- xmlParse(files) 
        curriculo <- xmlToList(xmltoparse, addAttributes = TRUE)
        curriculo$id <- as.character(strsplit(files[[1]],'\\.')[[1]][1])

    }else{
        for(i in seq_along(files)){
            xmltoparse <- xmlParse(files[i]) 
            curriculo[[i]] <- xmlToList(xmltoparse, addAttributes = TRUE)
            curriculo[[i]]$id <- as.character(strsplit(files[[i]],'\\.')[[1]][1])
        }
    }

    if(path!='.'){ setwd(path_old) }

    return(curriculo)
}
