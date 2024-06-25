#' Split a RasterLayer with no missing values
#' 
#' @param          x Raster* object with missing values
#' @param outputPath character with full path name of a directory where the splits
#'                   of \code{x} will be saved.
#' @param          h numeric, parts in which number of columns of \code{x} will be
#'                   splitted.
#' @param          v numeric, parts in which number of rows of \code{x} will be
#'                   splitted.
#' @param       name character with the basename to assign to output product.
#' 
#' @export
#' 
#' @importFrom sta getMaster
#' @importFrom geoTS split_replace
#' 
#' @seealso \code{\link[sta]{getMaster}}
#' 
#' @return At the location specified by \code{outputPath} there 
#' will be \code{n} \emph{GTiff} files, where \code{n=h*v}.
#' 
split_master <- function(x, outputPath, h, v, name="master"){
  if(missing(x) | missing(outputPath)){
    stop("'path' and 'outputPath' must be provided")
  }
  
  # MASTER <- getMaster(x=x)
  MASTER <- x
  MASTER[is.na(MASTER)] <- 1L
  
  # colCELL <- ncol(MASTER)/h
  # rowCELL <- nrow(MASTER)/v
  
  split_replace_terra(raster=MASTER,
                      h=h, v=v,
                      outputPath=outputPath,
                      name=name)
}