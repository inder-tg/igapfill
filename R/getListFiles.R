#' Lists of filenames of images
#' 
#' An application of \code{\link[base]{list.files}}, this function returns
#' a list containing ordered names of files to be processed with \code{\link[igapfill]{get_4Darray}}.
#' 
#' @param pattern character indicating file extension of interest
#' @param     ... additional parameters to be passed to \code{\link[base]{list.files}}
#' 
#' @export
#'
#' @importFrom gtools mixedsort
#' 
#' @note Outside the scope of the current package,
#' we recommend to employ \code{\link[base]{list.files}} in combination with
#' \code{\link[gtools]{mixedsort}} to obtain similar results.
#' 
#' @seealso \code{\link[igapfill]{create_dirs}}, \code{\link[igapfill]{get_4Darray}}
#' 
#' @return An ordered list
#' 
getListFiles <- function(pattern, ...){
  myFiles <- list.files(pattern = pattern, ...)
  mixedsort(myFiles)
}
