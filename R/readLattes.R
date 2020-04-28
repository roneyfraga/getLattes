#' @title readLattes
#' @description Import Lattes exported XML to R list.
#' @param filexml XML file exported from Lattes.
#' @return list
#' @details filexml XML file locally stored 
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # to import only one file
#' readLattes(filexml='6380212729787758.xml')
#' # to import several files
#' readLattes(filexml=list.files(pattern='.xml'))
#' }
#' @rdname readLattes
#' @export 
#' @importFrom XML xmlParse xmlToList
readLattes <- function(filexml){

    curriculo <- vector('list', length(filexml))

    if( length(filexml)==1){
        xmltoparse <- xmlParse(filexml) 
        curriculo <- xmlToList(xmltoparse, addAttributes = TRUE)
        curriculo$id <- as.character(strsplit(filexml[[1]],'\\.')[[1]][1])

    }else{
        for(i in seq_along(filexml)){
            xmltoparse <- xmlParse(filexml[i]) 
            curriculo[[i]] <- xmlToList(xmltoparse, addAttributes = TRUE)
            curriculo[[i]]$id <- as.character(strsplit(filexml[[i]],'\\.')[[1]][1])
        }
    }
    return(curriculo)
}
