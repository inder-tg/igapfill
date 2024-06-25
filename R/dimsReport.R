#' Dimensions and splitting characteristics of images to process
#' 
#' This function returns a few messages at the console. These messages
#' report on \emph{(i)} the dimensions of the images located at \code{path} and 
#' \emph{(ii)} the ways in which these dimensions can be splitted.
#' 
#' @param    path character, full path indicating the directory containing the images
#'                to process with \code{\link[gapfill]{Gapfill}}.
#' @param     mes character, when not provided the default message is 
#'                \emph{"The images located at 'path' have:"}.
#'                
#' @export
#' 
#' @importFrom raster stack
#' @importFrom raster nrow
#' @importFrom raster ncol
#' @importFrom numbers divisors
#' 
#' @seealso \code{\link[numbers]{divisors}}, \code{\link[igapfill]{waysToSplit}}
#' 
#' @return At the console there will be a series of messages, no further actions will be
#' taken.
#' 
dimsReport <- function(path, mes){
  
  pathFILES <- list.files(path = path, pattern = ".tif", 
                          full.names = TRUE)
  STACK <- stack(pathFILES)
  
  div_nrow <- divisors(nrow(STACK))
  div_ncol <- divisors(ncol(STACK))
  
  if(missing(mes)){
    mes <- message(paste0("The images located at ", path, " have:"))
  }
  
  message(mes)
  cat(textColor("nROW", "red"), "=", nrow(STACK), "\n")
  cat(textColor("nCOL", "green"), "=", ncol(STACK), "\n")

  message("FYI:")
  cat(textColor("nROW", "red"), "can be splitted in", div_nrow, "equal parts\n")
  cat(textColor("nCOL", "green"), "can be splitted in", div_ncol, "equal parts\n")
}
