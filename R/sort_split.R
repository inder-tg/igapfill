#' Console-based application to sort and split objects linked to images
#' 
#' An application of \code{\link[geoTS]{split_replace}} to configure/split/divide 
#' images in parts to which a subsequent call of \code{\link[igapfill]{applyGapfill}} 
#' can be easily handled by regular computer systems.
#' 
#' This function asks the user a series of inputs on-the-fly. Should the user allow it, 
#' these inputs will be used as arguments in a subsequent call to \code{split_replace}.
#' 
#' @param               path character with full path name to a directory containing a set
#'                           of files to be splitted.
#' @param          startYear numeric indicating the starting time-point, on the annual scale,
#'                           of a time series of satellite images.
#' @param            endYear number indicating the endind time-point, on the anual scale,
#'                           of a time series of satellite images.
#' @param parallelProcessing logical. Should parallel computing be used to split each layer
#'                           within the Raster* object generated from \code{path}? Default is
#'                           \code{FALSE}.
#' @param           numCores numeric. How many cores should be employed in parallel computing?
#'                           Only relevant when \code{parallelProcessing=TRUE}.
#'                  
#' @export                  
#'  
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar                
#' @importFrom gtools mixedsort 
#' @importFrom terra rast
#' @importFrom terra ncol
#' @importFrom terra nrow
#' @importFrom terra ncell
#' @importFrom terra nlyr
#' @importFrom terra as.polygons
#' @importFrom terra ext
#' @importFrom terra writeRaster
#' @importFrom terra datatype
#' @importFrom terra add<-
#' @importFrom raster rasterOptions
#' @importFrom utils globalVariables
#' 
#' @details \code{\link[igapfill]{create_dirs}} defines a specific directory
#' structure used by \code{sort_split}, hence, it is highly recommended
#' to use \code{create_dirs} in advance. Also, it is highly recommended to use 
#' \code{sort_split} before using \code{\link[igapfill]{applyGapfill}}.
#' 
#' @note The "sort" part of the function means that in case that the file 
#' list created from \code{path} is not originally ordered, then \code{sort_split}
#' will sort it out internally.
#' 
#' @seealso \code{\link[igapfill]{waysToSplit}}, \code{\link[igapfill]{dimsReport}}.
#' 
#' @return When the users decide not to proceed with this function,
#' the message \emph{"Process stopped with no action taken"} is returned at the console. 
#' Otherwise, the resulting splits (.tif files) will be saved at the sub-directories 
#' defined by \code{paste0(path, "/gapfill/splits")}.
#' 
sort_split <- function(path, startYear, endYear,
                       parallelProcessing=FALSE, 
                       numCores=4){
  yearsToFill <- startYear:endYear
  
  pathFILES <- mixedsort(list.files(path=path,
                                    pattern = ".tif",
                                    full.names = TRUE))
  
  if(length(yearsToFill)^2 != length(pathFILES)){
    stop(paste0("Number of files in ", path, 
                " must be equal to length(startYear:endYear)^2"))
  }
  
  cat(textColor("The following files:", "red"), "\n")
  
  for(i in 1:length(pathFILES)){
    cat( "(",i,")", pathFILES[i] , "\n" )
  }
  
  cat(textColor("will be splitted under the following scheme", "red"), "\n")
  
  print_years_array(p=length(yearsToFill), years=yearsToFill)
  
  dimsReport(path=path, mes = "The images in these STACKs have:")
  
  message("How would you like to proceed?")
  
  stackLIST <- list()
  p <- length(yearsToFill)
  for(k in 1:p){
    # stackLIST[[k]] <- stack( pathFILES[1:p + (k-1)*p] )
    stackLIST[[k]] <- rast( pathFILES[1:p + (k-1)*p] )
  }
  
  v <- as.numeric(readline("Parts to split nROW, v: "))
  h <- as.numeric(readline("Parts to split nCOL, h: "))
  waysToSplit(h=h,v=v, raster=stackLIST[[1]])
  
  colCELL <- ncol(stackLIST[[1]])/h
  rowCELL <- nrow(stackLIST[[1]])/v
  
  response <- readline("Would you like to proceed with these parameters [Y/n]: ")
  
  if(response == "yes" | response == "Yes" | response == "Y" | response == "y"){
    
    dir_response <- readline("Should I use the directories suggested in my documentation to save outputs [Y/n]: ")
    
    if(dir_response == "yes" | dir_response == "Yes" | dir_response == "Y" | dir_response == "y"){
      output_dir <- paste0(path, "/gapfill/splits")
      output_dir_master <- paste0(path, "/gapfill/master")
    } else {
      output_dir <- readline("Please type in output directory (splits): ")
      output_dir_master <- readline("Please type in output directory (master): ")
    }
    
    outputPath_split <- list.dirs(path=output_dir)[-1]
    for(k in 1:p){
      k_text <- paste0("Baseline name for cells of STACK_", k, ": ")
      name_split <- readline(k_text)
      split_replace_terra(raster=stackLIST[[k]],
                          h=colCELL,
                          v=rowCELL,
                          outputPath=outputPath_split[k],
                          name=name_split,
                          parallelProcessing=parallelProcessing,
                          numCores=numCores)
    }
    
    message("Next, splitting master")
    MASTER <- rast(pathFILES[1])
    MASTER[is.na(MASTER)] <- 1L
    # writeRaster(MASTER, 
    #             filename = , 
    #             datatype="INT2S", overwrite=TRUE)
    split_replace_terra(raster=MASTER, 
                        h=colCELL, v=rowCELL,
                        outputPath=output_dir_master,
                        name = "master", parallelProcessing = parallelProcessing,
                        dataType="INT4S", numCores=numCores)
    
  } else {
    return("Process stopped with no action taken")
  }
}
