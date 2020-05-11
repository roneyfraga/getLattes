#' @title linearizeNestedList
#'
#' @description Implements a recursive algorithm to linearize nested lists upto any arbitrary level of nesting.
#' @param nList The input \code{list}.
#' @param linearizeDataFrames Logical. TRUE the columns in \code{data.frame}s will the "linearized". Default \code{FALSE}.
#' @param nameSep Character to be the separation between names.  Default  "/".
#' @param forceNames Logical. TRUE will rename list elements. Default \code{FALSE}.
#' @author Akhil S Bhel
#'
#' @details
#' Implements a recursive algorithm to linearize nested lists upto any
#' arbitrary level of nesting (limited by R's allowance for recursion-depth).
#' By linearization, it is meant to bring all list branches emanating from
#' any nth-nested trunk upto the top-level trunk s.t. the return value is a
#' simple non-nested list having all branches emanating from this top-level
#' branch.
#' 
#' Since dataframes are essentially lists a boolean option is provided to
#' switch on/off the linearization of dataframes. This has been found
#' desirable in the author's experience.
#' Also, one'd typically want to preserve names in the lists in a way as to
#' clearly denote the association of any list element to it's nth-level
#' history. As such we provide a clean and simple method of preserving names
#' information of list elements. The names at any level of nesting are
#' appended to the names of all preceding trunks using the `nameSep` option
#' string as the seperator. The default `/` has been chosen to mimic the unix
#' tradition of filesystem hierarchies. The default behavior works with
#' existing names at any n-th level trunk, if found; otherwise, coerces simple
#' numeric names corresponding to the position of a list element on the
#' nth-trunk. Note, however, that this naming pattern does not ensure unique
#' names for all elements in the resulting list. If the nested lists had
#' non-unique names in a trunk the same would be reflected in the final list.
#' Also, note that the function does not at all handle cases where `some`
#' names are missing and some are not.
#' Clearly, preserving the n-level hierarchy of branches in the element names
#' may lead to names that are too long. Often, only the depth of a list
#' element may only be important. To deal with this possibility a boolean
#' option called `forceNames` has been provided. forceNames shall drop all
#' original names in the lists and coerce simple numeric names which simply
#' indicate the position of an element at the nth-level trunk as well as all
#' preceding trunk numbers.
#' @seealso \code{\link{unlist}}
#' @references
#' \url{https://sites.google.com/site/akhilsbehl/geekspace/articles/r/linearize_nested_lists_in_r}
#' @examples
#' \dontrun{
#' if(interactive()){
#' ml <- list(a = "a", 
#'           b = 1:3, 
#'           c = data.frame(x = runif(3), y = runif(3)), 
#'           d = list(a = list("a", "b"), 
#'                    b = list(list(1), list(2)))
#'  
#' 
#' linearizeNestedList(ml)
#' linearizeNestedList(ml, linearizeDataFrames = TRUE)
#' linearizeNestedList(ml, forceNames = TRUE)
#' linearizeNestedList(ml, forceNames = TRUE, nameSep = "_")
#' }
#' }
#' @export linearizeNestedList
linearizeNestedList <- function(nList, linearizeDataFrames=FALSE, nameSep="/", forceNames=FALSE){
    #
    # Sanity checks:
    #
    stopifnot(is.character(nameSep), length(nameSep) == 1)
    stopifnot(is.logical(linearizeDataFrames), length(linearizeDataFrames) == 1)
    stopifnot(is.logical(forceNames), length(forceNames) == 1)
    if (! is.list(nList)) return(nList)
    #
    # If no names on the top-level list coerce names. Recursion shall handle
    # naming at all levels.
    #
    if (is.null(names(nList)) | forceNames == TRUE)
        names(nList) <- as.character(1:length(nList))
    #
    # If simply a dataframe deal promptly.
    #
    if (is.data.frame(nList) & linearizeDataFrames == FALSE)
        return(nList)
    if (is.data.frame(nList) & linearizeDataFrames == TRUE)
        return(as.list(nList))
    #
    # Book-keeping code to employ a while loop.
    #
    A <- 1
    B <- length(nList)
    #
    # We use a while loop to deal with the fact that the length of the nested
    # list grows dynamically in the process of linearization.
    #
    while (A <= B) {
        Element <- nList[[A]]
        EName <- names(nList)[A]
        if (is.list(Element)) {
            #
            # Before and After to keep track of the status of the top-level trunk
            # below and above the current element.
            #
            if (A == 1) {
                Before <- NULL
            } else {
                Before <- nList[1:(A - 1)]
            }
            if (A == B) {
                After <- NULL
            } else {
                After <- nList[(A + 1):B]
            }
            #
            # Treat dataframes specially.
            #
            if (is.data.frame(Element)) {
                if (linearizeDataFrames == TRUE) {
                    #
                    # `Jump` takes care of how much the list shall grow in this step.
                    #
                    Jump <- length(Element)
                    nList[[A]] <- NULL
                    #
                    # Generate or coerce names as need be.
                    #
                    if (is.null(names(Element)) | forceNames == TRUE)
                        names(Element) <- as.character(1:length(Element))
                    #
                    # Just throw back as list since dataframes have no nesting.
                    #
                    Element <- as.list(Element)
                    #
                    # Update names
                    #
                    names(Element) <- paste(EName, names(Element), sep=nameSep)
                    #
                    # Plug the branch back into the top-level trunk.
                    #
                    nList <- c(Before, Element, After)
                }
                Jump <- 1
            } else {
                nList[[A]] <- NULL
                #
                # Go recursive! :)
                #
                if (is.null(names(Element)) | forceNames == TRUE)
                    names(Element) <- as.character(1:length(Element))
                Element <- linearizeNestedList(Element, linearizeDataFrames,
                                               nameSep, forceNames)
                names(Element) <- paste(EName, names(Element), sep=nameSep)
                Jump <- length(Element)
                nList <- c(Before, Element, After)
            }
        } else {
            Jump <- 1
        }
        #
        # Update book-keeping variables.
        #
        A <- A + Jump
        B <- length(nList)
    }
    return(nList)
}
