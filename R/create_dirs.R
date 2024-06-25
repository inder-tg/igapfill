#' Creates a set of directories which are an essential part of the general workflow
#' of this package
#' 
#' Using the directory provided by \code{path} as root this function
#' creates the sub-directory \emph{/gapfill} with sub-folders \emph{/gapfill/filled}, 
#' \emph{/gapfill/master}, \emph{/gapfill/output}, \emph{/gapfill/progressReports} and \emph{/gapfill/splits}.
#' 
#' @param      path character, full path indicating the directory containing the images
#'                  to process with \code{\link[gapfill]{Gapfill}}.
#' @param startYear numeric, indicates the starting time-point, on the annual scale, 
#'                  of a time series of satellite images to process.
#' @param   endYear numeric, indicates the ending time-point, on the annual scale, 
#'                  of a time series of satellite images to process.
#' 
#' @export
#' 
#' @details At \emph{/gapfill/splits} this function will create \code{n} additional
#' folders, where \code{n=length(startYear:endYear)}. These folders will be the recipients 
#' of the output of \code{\link[igapfill]{sort_split}} when this function is applied
#' to the images to process with \code{\link[gapfill]{Gapfill}}.
#' 
#' @seealso \code{\link[igapfill]{sort_split}}, \code{\link[geoTS]{split_replace}}
#' 
#' @return At the location indicated by \code{path}, the abovementioned directories
#' will be created.
#' 
create_dirs <- function(path, startYear, endYear){
  dir.create( path = paste0(path, "/gapfill") )
  dir.create( path = paste0(path, "/gapfill/output") )
  dir.create( path = paste0(path, "/gapfill/filled") )
  dir.create( path = paste0(path, "/gapfill/splits") )
  dir.create( path = paste0(path, "/gapfill/master") )
  
  for(year in startYear:endYear){
    dir.create( path = paste0(path, "/gapfill/splits/", year) )
  }
  dir.create( path = paste0(path, "/gapfill/progressReports") )

outputMessage <- paste0( "Done, check output at: ", textColor(text=path, color = "red") )
message(outputMessage)
}
