#' Brief report on ways to split a Raster* object
#' 
#' Based on the passed arguments, this function returns three messages
#' at the console. First, the number of \emph{splits} for the Raster* object.
#' Second, the number of rows and third, the number of columns of each split.
#' 
#' @param      h numeric, parts in which number of columns of \code{raster} will be
#'               splitted
#' @param      v numeric, parts in which the number of rows of \code{raster} will be
#'               splitted
#' @param raster Raster* object to be splitted
#' 
#' @export
#' 
#' @importFrom raster ncol
#' @importFrom raster nrow
#' 
#' @details For an abuse of language, here we use the term \emph{split} to signify
#' \emph{cell} or \emph{crop}, which in this context might be more common terms.
#' 
#' @return At the console, there will be a summary indicating the number of \emph{splits}
#' for the Raster* object as well as the number of rows and columns of each \emph{split}.
#' 
waysToSplit <- function(h,v,raster){
  colCELL <- ncol(raster)/h
  rowCELL <- nrow(raster)/v
  
  message("With these arguments, there will be:")
  
  splitText <- paste(h*v, "splits")
  cat(textColor(splitText, "blue"), "each having\n")
  cat(textColor("nROW", "red"), "=", rowCELL, "\n")
  cat(textColor("nCOL", "green"), "=", colCELL, "\n")
}