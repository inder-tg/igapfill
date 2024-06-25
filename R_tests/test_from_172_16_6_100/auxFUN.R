
library(raster)
library(numbers)
library(gtools)
library(gapfill)
library(parallel)
library(foreach)
library(doParallel)
library(geoTS)
library(gtools)


#' @param      path character, full path indicating the directory containing the images
#'                  to fill
#' @param startYear numeric, indicates the starting time-point with respect to temporal
#'                  span of the images to fill  
#' @param   endYear numeric, indicates the ending time-point with respect to temporal
#'                  span of the images to fill 
#'                  

create_dirs <- function(path, startYear, endYear){
  dir.create( path = paste0(path, "/gapfill") )
  dir.create( path = paste0(path, "/gapfill/output") )
  dir.create( path = paste0(path, "/gapfill/filled") )
  dir.create( path = paste0(path, "/gapfill/splits") )
  for(year in startYear:endYear){
    dir.create( path = paste0(path, "/gapfill/splits/", year) )
  }
  dir.create( path = paste0(path, "/gapfill/progressReports") )
}

#' @param nodata_value 
#' @param       Raster 
#' 

set_NoData <- function(nodata_value, Raster){
  Raster[[ Raster == nodata_value ]] <- NA
}

textColor <- function(text, color = c("red", "green", 
                                      "yellow", "blue")){
  
  color <- match.arg(color)
  
  if(color == "red"){
    col <- 31
  }
  
  if(color == "green"){
    col <- 32
  }
  
  if(color == "yellow"){
    col <- 33
  }
  
  if(color == "blue"){
    col <- 34
  }
  
  paste0("\033[0;", col, "m", text,"\033[0m")
  
}

#' @param path character, full path indicating the directory containing the images
#'                  to fill
#' 
#' @importFrom stack raster
#' @importFrom divisor numbers
#' 
dimsReport <- function(path, message){
  # path <- paste0( getwd(), "/original" )
  pathFILES <- list.files(path = path, pattern = ".tif", 
                          full.names = TRUE)
  STACK <- stack(pathFILES)
  
  div_nrow <- divisors(nrow(STACK))
  div_ncol <- divisors(ncol(STACK))
  
  if(missing(message)){
    message <- message(paste0("The images located at ", path, " have:"))
  }
  
  message(message)
  cat(textColor("nROW", "red"), "=", nrow(STACK), "\n")
  cat(textColor("nCOL", "green"), "=", ncol(STACK), "\n")
  # cat("\033[0;34m===\033[0m\n")
  message("FYI:")
  cat(textColor("nROW", "red"), "can be splitted in", div_nrow, "equal parts\n")
  cat(textColor("nCOL", "green"), "can be splitted in", div_ncol, "equal parts\n")
}

#' @param      h numeric, parts in which number of columns of \code{raster} will be
#'               split
#' @param      v numeric, parts in which the number of rows of \code{raster} will be
#'               split
#' @param raster Raster* to be split
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

print_years_array <- function(p, years){
  what_to_print <- sapply(1:p^2, function(s) 
    paste0("(", s, ")"))
  
  for(k in 1:p){
    cat(years[k],
        paste0("STACK_", k, ":"),
        what_to_print[1:p + (k-1)*p], "\n")
  }
}


#' @param      path character with full path name to a directory containing a set
#'                  of files to be split
#' @param startYear numeric indicating the starting time-point, on the annual scale,
#'                  of a time series of satellite images
#' @param   endYear number indicating the endind time-point, on the anual scale,
#'                  of a time series of satellite images
#'                  
#' @importFrom mixedsort gtools
#' 
sorting_splitting <- function(path, startYear, endYear,
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
  
  dimsReport(path=path, message = "The images in these STACKs have:")
  
  message("How would you like to proceed?")
  
  stackLIST <- list()
  p <- length(yearsToFill)
  for(k in 1:p){
    stackLIST[[k]] <- stack( pathFILES[1:p + (k-1)*p] )
  }
  
  v <- as.numeric(readline("Parts to split nROW, v:"))
  h <- as.numeric(readline("Parts to split nCOL, h:"))
  waysToSplit(h=h,v=v, raster=stackLIST[[1]])
  
  colCELL <- ncol(stackLIST[[1]])/h
  rowCELL <- nrow(stackLIST[[1]])/v
  
  response <- readline("Would you like to proceed with these parameters [yes, no]:")

  if(response == "yes"){
    
    dir_response <- readline("Do I use input directory to save outputs [yes, no]:")

    if(dir_response == "yes"){
      output_dir <- path
    } else {
      output_dir <- readline("Please type in output directory:")
    }
        
    # message("Creating auxiliary folders")
    # create_dirs(path=output_dir, startYear=startYear, endYear=endYear)
    
    # outputPath_split <- list.dirs(path=paste0(output_dir, "/gapfill/splits"))[-1]
    outputPath_split <- list.dirs(path=output_dir)[-1]
    for(k in 1:p){
      k_text <- paste0("Baseline name for cells of STACK_", k, ":")
      name_split <- readline(k_text)
      split_replace(raster=stackLIST[[k]],
                    h=colCELL,
                    v=rowCELL,
                    outputPath=outputPath_split[k],
                    name=name_split,
                    parallelProcessing=parallelProcessing,
                    numCores=numCores)
    }
    
  } else {
    return("Process stopped with no action taken")
  }
}

#' @param path character with full path name of a directory containing files that
#'             can be read as RasterStacks
#'             
#' @importFrom raster stack
#' @importFrom   base array
#' @importFrom raster getValues
#'             
get_3Darray <- function(path){
  STACK <- stack(path)
  ARRAY <- array(NA, dim = c(nrow(STACK), ncol(STACK), nlayers(STACK)))
  for( i in 1:nlayers(STACK) ){
    TEMP <- getValues(subset(STACK, i))
    ARRAY[,,i] <- TEMP
  }
  ARRAY
}

#' @param stack RasterStack 
#' 
get_LON <- function(stack){
  as.character( seq(xmin(stack), xmax(stack), by = res(stack)[1] ) )[-1]
}

#' @param stack RasterStack 
#' 
get_LAT <- function(stack){
  as.character( seq(ymin(stack), ymax(stack), by = res(stack)[1] ) )[-1]
}

#' @param listLayer list, containing lists with names of files to be assembled as a 4D array.
#' @param       lon character, vector whose entries indicate longitude coordinates.
#' @param       lat character, vector whose entries indicate latitude coordinates. 
#' @param      days numeric, vector indicating what DoYs are being considered. Length of this
#'                  object must be equal to length of \code{listLayer}. 
#' @param     years integer, vector indicating what years are being considered. Length of this
#'                  object must be equal to length of \code{listLayer}.
#' 
#' @return An array of 4 dimensions: longitud, latitude, days and years
#' 
#' @seealso \code{get_LON}, \code{get_LAT}, \code{getListFiles}
#' 
get_array_lat_lon_day_year <- function(listLayer, lon, lat, days, years){
  
  array_lat_lon_days_years <- array(NA, 
                                    dim = c(length(lat), length(lon), length(listLayer), length(listLayer)),
                                    dimnames = list(lat, lon, days, years))
    
  for(i in 1:length(listLayer)){
    for(j in 1:length(listLayer)){
      array_lat_lon_days_years[,,j,i] <- listLayer[[i]][,,j]
    }
  }
  
  array_lat_lon_days_years
}

#' @param pattern character indicating file extension of interest
#' @param     ... additional parameters to be used by \code{\link[base]{list.files}}
#' 
#' @importFrom gtools mixedsort
#' 
getListFiles <- function(pattern, ...){
  myFiles <- list.files(pattern = pattern, ...)
  mixedsort(myFiles)
}

# listPath es una lista conteniendo nombres de archivos agrupados por years
# years can be computed as startYear:endYear
# days must be provided by the user, otherwise it is set to 1:length(years)

#' @param listPath list, containing lists with names of files to be assembled as a 4D array.
#' @param        i numeric indicating \eqn{i}-th file to be processed
#' @param      lon character, vector whose entries indicate longitude coordinates.
#' @param      lat character, vector whose entries indicate latitude coordinates.
#' @param     days numeric, vector indicating what DoYs are being considered. Length of this
#'                 object must be equal to length of \code{listPath}.
#' @param    years integer, vector indicatins what years are being considered. Length of this
#'                 object must be equal to length of \code{listPath}.
#'                 
#' @details Each entry of \code{listPath} must contain files from the same year. 
#' \code{lon} and \code{lat} can be obtained with the functions \code{get_LON} and
#' \code{get_LAT}, respectively. \code{days} must be provided by the user, otherwise
#' it will be set to \code{1:length(years)}.
#'                 
#' @importFrom base vector
#' 
#' @return An array of 4 dimensions: longitud, latitude, days and years
#' 
#' @seealso \code{get_LON}, \code{get_LAT}, \code{getListFiles}
#' 
get_4Darray <- function(listPath, i, lon, lat, days, years){ #, longitude, latitude, days, years){
  
  layerList <- vector(mode="list", length=length(listPath))
  
  for(k in 1:length(listPath)){
    layerList[[k]] <- get_3Darray(path=listPath[[k]][i])
  }

  get_array_lat_lon_day_year(listLayer=layerList, lon=lon, lat=lat, days=days, years=years)
}

# listFILES2014 <- getListFiles(pattern=".tif", 
#                               path=paste0( getwd(), "/gapfill/splits_test/2014"), 
#                               full.names=TRUE)
# 
# listFILES2015 <- getListFiles(pattern=".tif", 
#                               path=paste0( getwd(), "/gapfill/splits_test/2015"), 
#                               full.names=TRUE)
# 
# listFILES2016 <- getListFiles(pattern=".tif", 
#                               path=paste0( getwd(), "/gapfill/splits_test/2016"), 
#                               full.names=TRUE)
# 
# totalListFILES <- list(listFILES2014, listFILES2015, listFILES2016)
# 
# test <- list.dirs(path = paste0( getwd(), "/gapfill/splits_test"))[-1]
# 
# TEMP <- as.integer(sapply( 1:length(test), function(s) {temp <- unlist(strsplit(test[[s]], "/")); 
# temp[length(temp)]} ))

get_applyGapfill <- function(inputDir, outputDir, progressDir, 
                             lat, lon, days, years, 
                             numCores=6, scale=1e-4, 
                             clipRange=c(-1,1),
                             addArgToReport=TRUE){
  
  # inputDir = "/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/images/gapfill/splits_tests"
  # outputDir = "/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/images/gapfill/output"
  # progressDir = "/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/images/gapfill/progressReports"
  # lat=LATITUDE
  # lon = LONGITUDE
  # days = DAYS
  # years = YEARS
  
  yearsToProcess <- list.dirs(path = inputDir)[-1]
  
  TEMP <- as.integer(sapply( 1:length(yearsToProcess), 
                  function(s) {temp <- unlist(strsplit(yearsToProcess[[s]], "/")); 
                  temp[length(temp)]} ))
  
  if( !identical(x=TEMP, y=years) ){
    text1 <- paste("Folder names in 'inputDir' must coincide with 'years' entries.", "\n")
    text2 <- paste("You can check out", 
                   textColor(text="create_dirs()", color = "blue"), "for a possible solution.")
    stop(paste(text1, text2))
  }
  
  totalListFILES <- vector(mode="list", length=length(yearsToProcess))
  for(i in 1:length(yearsToProcess)){
    totalListFILES[[i]] <- getListFiles(pattern = ".tif", 
                                        path=yearsToProcess[i], 
                                        full.names=TRUE)
  }
  
  layersToProcess <- length(totalListFILES[[1]])
  
  cluster <- parallel::makeCluster(numCores, outfile="")
  registerDoParallel(cluster)
  name_RData_output <- paste0( outputDir, "/gapfill_output_cell_" )
  
  if(addArgToReport){
    write("==========",
          paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    TEXT <- "Arguments: "
    write(TEXT,
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write("==========",
          paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    
    write(paste0("Input directory: ", inputDir),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("Output directory: ", outputDir),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("Progress Report directory: ", progressDir),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("lat (as vector): first & last entries: ", lat[1], 
                 ", ", lat[length(lat)], 
                 "; length equal to ", length(lat)),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("lon (as vector): first & last entries: ", lon[1], 
                 ", ", lon[length(lon)],
                 "; length equal to ", length(lon)),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("days (as vector, only first day of each year): first entry ", days[1], 
                 ", last entry ", days[length(days)],
                 ", length equal to ", length(days)),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("years: ", years[1], ":", years[length(years)]),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("Number of cores in used: ", numCores),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("Scale factor: ", scale),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
    write(paste0("Clip range: (", clipRange[1], ",", clipRange[2], ")"),
          file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
  }
  
  write("===============================",
        paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
  write(paste0("Started at: ", as.character(Sys.time()[1])),
        file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
  write("===============================",
        paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
  
  
  
  output <- foreach( i = 1:layersToProcess,
                     .export = c("get_3Darray", "get_array_lat_lon_day_year", "get_4Darray"),
                     .packages = c("gapfill", "raster") ) %dopar% {
                       
    ARRAY_LAT_LONG_DAYS_YEARS <- get_4Darray(listPath = totalListFILES, i = i,
                                             lat = lat, lon = lon,
                                             days = days, years = years)

    output_gapFill <- Gapfill(data = ARRAY_LAT_LONG_DAYS_YEARS * scale,
                              clipRange = clipRange)

    save(output_gapFill, file = paste0(name_RData_output, i, ".RData"))
    if(i %% numCores == 0){
      text <- paste0("Working on cell ", i)
      write(text, file = paste0( progressDir, "/gapfill_progress.txt"),
            append = TRUE)
    }
  }
  stopCluster(cluster)
  write("===============================",
        paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
  write( paste0("Ended at: ", as.character(Sys.time()[1])),
         file = paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
  write("===============================",
        paste0( progressDir, "/gapfill_progress.txt"), append = TRUE)
  
  outputMessage <- paste0( "See ", textColor(text=outputDir, color = "red") )
  message(outputMessage)
}

# ---

applyGapfill <- function(inputDir, outputDir, progressDir, 
                         lat, lon, days, years, 
                         numCores = 6, scale = 1e-4, 
                         clipRange = c(-1,1),
                         addArgToReport=TRUE){
  
  # inputDir = "/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/images/gapfill/splits_tests"
  # outputDir = "/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/images/gapfill/output"
  # progressDir = "/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/images/gapfill/progressReports"
  # lat=LATITUDE
  # lon = LONGITUDE
  # days = DAYS
  # years = YEARS
  # numCores = 6
  # scale = 1e-4
  # clipRange=c(-1,1)
  

  # if(missing(inputDir) | missing(outputDir) | missing(progressDir) ){
  #   message("this is a test")
  # }
  
  if(missing(lat)){
    stop("'lat' must be provided")
  }
  
  if(missing(lon)){
    stop("'lon' must be provided")
  }
  
  if(missing(days)){
    stop("'days' must be provided")
  }
  
  if(missing(years)){
    stop("'years' must be provided")
  }
    
  if( missing(inputDir) | missing(outputDir) | missing(progressDir) ){
    message("Arguments 'inputDir', 'outputDir' and 'progressDir' must be provided")
    question_i <- readline("Have you used create_dirs() to get these parameters before? [yes/no]:")

    if( question_i == "yes" ){
      stop("Try again later passing the required parameters")
    } else {
      message("I will use create_dirs() to generate 'inputDir', 'outputDir' and 'progressDir'")
      message("I require a 'Path' to help you get these parameters")
      # message("Beware that information on 'Path' may be affected by what follows")
      message("I strongly recommend that 'Path' does not contain any folder or file")
      question_ii <- readline("Do you want my help to get these parameters now? [yes/no]:")

      if( question_ii == "yes" ) {
        
        PATH <- readline("Path:")
        
        if(!dir.exists(PATH)){
          stop(paste0(PATH, " does not exists"))
        }
        
        START_YEAR <- as.numeric(readline("First year:"))
        END_YEAR <- as.numeric(readline("Last year:"))
        
        create_dirs(path=PATH, startYear = START_YEAR, endYear = END_YEAR)
        inputDir <- paste0(PATH, "/splits")
        outputDir <- paste0(PATH, "/output")
        progressDir <- paste0(PATH, "/progressReports")
      } else {
        stop("Try again later passing the required parameters")
      }
    }
  }
  
  get_applyGapfill(inputDir=inputDir,
                   outputDir=outputDir,
                   progressDir=progressDir,
                   lat=lat, lon=lon, days=days, years=years,
                   numCores=numCores, scale=scale, 
                   clipRange=clipRange,
                   addArgToReport=addArgToReport) 
}

# appApplyGapfill <- function(path, startYear, endYear, 
#                             dirInput, dirOutput, dirProgress,
#                             lat, lon, days, years, numCores=6, 
#                             scale=1e-4, clipRange=c(-1,1)){
#   
#   if(missing(path)){
#     stop("'path' must be provided")
#   }
#   
#   sorting_splitting(path = path,
#                     startYear = startYear, endYear = endYear)
#   
#   applyGapfill(inputDir = dirInput,
#                outputDir = dirOutput,
#                progressDir = dirProgress,
#                lat = lat, lon = lon, days = days, years = years, 
#                numCores = numCores,
#                scale = scale, clipRange = clipRange)
# }

gappfill <- function(){
  
  PATH <- readline("Full path (directory containing images to process): ")
  
  if(!dir.exists(PATH)){
    stop(paste0(PATH, " does not exists"))
  }
  
  question_i <- readline("Have you set up this directory according with my documentation? [yes|no] ")
  
  if(question_i=="no"){
    question_ii <- readline("Do you want me to set up your working directory now? [yes|no] ")
    
    if(question_ii=="no"){
      stop("Process stopped with no action taken")
    } else {
      START_YEAR <- as.numeric(readline("First year (to process): "))
      END_YEAR <- as.numeric(readline("Last year (to process): "))
      
      create_dirs(path=PATH, startYear = START_YEAR, endYear = END_YEAR)
      inputDir <- paste0(PATH, "/splits")
      outputDir <- paste0(PATH, "/output")
      progressDir <- paste0(PATH, "/progressReports")
    }
 
  }
  
  question_iii <- readline("Have you splitted up your original images? [yes|no] ")
  
  if(question_iii=="no"){
    question_iv <- readline("Do you want me to take care of the splitting process? [yes|no] ")
    
    if(question_iv=="no"){
      stop("Process stopped with no action taken")
    } else {
      # splitsPATH <- readline("Full path (directory containing images to process):")
      START_YEAR <- as.numeric(readline("First year (to process): "))
      END_YEAR <- as.numeric(readline("Last year (to process): "))
      
      sorting_splitting(path = PATH, startYear = START_YEAR, endYear = END_YEAR)
    }
  } 
  
  # PATH <- paste0(getwd(),"/images")

  
  dirInput <- paste0(PATH, "/gapfill/splits")
  
  dirOutput <- paste0(PATH, "/gapfill/output")
  
  dirProgress <- paste0(PATH, "/gapfill/progressReports")
  
  START_YEAR <- as.numeric(readline("First year (to process): "))
  
  END_YEAR <- as.numeric(readline("Last year (to process): "))
  # path_2014 <- list.files(path = paste0( getwd(), 
  #                                        "/images/gapfill/splits/2014"),
  #                         pattern = ".tif",
  #                         full.names = TRUE)
  
  
  dirSPLITS <- list.dirs(path = dirInput)[-1]
  
  genPath_firstSplit <- list.files(path = dirSPLITS[1],
                          pattern = ".tif", 
                          full.names = TRUE)
  
  STACK <- stack(genPath_firstSplit[1])
  
  LONGITUDE <- get_LON(stack=STACK)
  
  LATITUDE <- get_LAT(stack=STACK)
  
  YEARS <- START_YEAR:END_YEAR
  
  DAYS <- numeric(length(YEARS))
  for(i in 1:length(DAYS)){
    DAYS[i] <- as.numeric(readline(paste0(i, "-th Day: ")))
  }
  
  NUMCORES <- as.numeric(readline("How many cores of your system do you want to use? "))
  question_scale <- readline("Do you want to re-scale the observations in your images? [yes|no]")
  
  if(question_scale=="no"){
    SCALE <- 1
  } else {
    SCALE <- as.numeric(readline("Scale: "))
  }
  
  question_clip <- readline("Do you want to change the clip range? [yes|no] ")
  
  if(question_clip=="no"){
    CLIP_RANGE <- c(-1,1)
  } else {
    CLIP_min <- as.numeric(readline("Lower limit: "))
    CLIP_max <- as.numeric(readline("Upper limit: "))
    CLIP_RANGE <- c(CLIP_min, CLIP_max)
  }

  cat( str(dirInput), "\n" )
  cat( str(dirOutput), "\n" )
  cat( str(dirProgress), "\n" )
  cat( str(LATITUDE), "\n" )
  cat( str(LONGITUDE), "\n" )
  cat( str(DAYS), "\n" )
  cat( str(YEARS), "\n" )
  cat( str(NUMCORES), "\n" )
  cat( str(SCALE), "\n" )
  cat( str(CLIP_RANGE), "\n" )
    
  # applyGapfill(inputDir = dirInput,
  #              outputDir = dirOutput,
  #              progressDir = dirProgress,
  #              lat = LATITUDE, lon = LONGITUDE,
  #              days = DAYS, years = YEARS,
  #              numCores = NUMCORES,
  #              scale = SCALE, clipRange = CLIP_RANGE,
  #              addArgToReport = ARGUMENTS)
}

# ---

getMASTER <- function(path, outputPath){
  
  if(missing(path) | missing(outputPath)){
    stop("'path' and 'outputPath' must be provided")
  }
  
  pathFILES <- list.files(path = path, 
                          pattern = ".tif", 
                          full.names = TRUE)
  STACK <- stack(pathFILES)
  
  MASTER <- subset(STACK,1)
  MASTER[is.na(MASTER)] <- 1

  writeRaster(x=MASTER,
              filename = paste0(outputPath, "/master"),
              format="GTiff", datatype="INT1U",
              overwrite=TRUE)
}

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  env
}

# --- Commented on November 13, 2023
# parallel_mosaicking <- function(inputDirImages,
#                                 inputDirRData,
#                                 inputDirMaster,
#                                 outputDir, 
#                                 progressReportDir,
#                                 numCores,
#                                 dataType="FLT4S"){
#   
#   # inputDirImages="/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/shantall"
#   # inputDirRData="/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/shantall/gapfill/output"
#   # inputDirMaster="/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/shantall/master"
#   # outputDir="/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/shantall/gapfill/filled"
#   # progressReportDir="/home/itecuapetla/Desktop/R/RPkgs_dev/applyGapfill/shantall/gapfill/progressReports"
#   # numCores=2
#   # dataType="FLT4S"
#   
#   imagesToProcess <- mixedsort(list.files(path=inputDirImages,
#                                 pattern = ".tif",
#                                 full.names = TRUE))
#   
#   RDataToProcess <- mixedsort(list.files(path = inputDirRData,
#                                pattern = ".RData",
#                                full.names = TRUE))
#   
#   masterToProcess <- mixedsort(list.files(path = inputDirMaster,
#                                 pattern = ".tif",
#                                 full.names = TRUE))
#   
#   totalListFILES <- sqrt(length(imagesToProcess))
# 
#   write("===============================",
#         paste0( progressReportDir, "/mosaicking_progress.txt"), 
#         append = TRUE)
#   write(paste0("Started at: ", as.character(Sys.time()[1])),
#         file = paste0( progressReportDir, "/mosaicking_progress.txt"), 
#         append = TRUE)
#   write("===============================",
#         paste0( progressReportDir, "/mosaicking_progress.txt"), 
#         append = TRUE)
#   
#   for(i in 1:totalListFILES){
#     for(j in 1:totalListFILES){
#       original <- raster(imagesToProcess[totalListFILES * (i-1) + j])
#       
#       message("Working on file ", names(original))
#       
#       write("-------------------------",
#             paste0( progressReportDir, "/mosaicking_progress.txt"), 
#             append = TRUE)
#       write(paste0("Working on file ", names(original)),
#             file = paste0( progressReportDir, "/mosaicking_progress.txt"), 
#             append = TRUE)
#       write("-------------------------",
#             paste0( progressReportDir, "/mosaicking_progress.txt"), 
#             append = TRUE)
#       
#       cluster <- parallel::makeCluster(numCores, outfile = "")
#       registerDoParallel(cluster)
#       
#       MOSAIC <- foreach(k = 1:length(RDataToProcess),
#                         .export = c("LoadToEnvironment"),
#                         .packages = c("raster","geoTS"))%dopar%{
#                           
#                           cell <- LoadToEnvironment(RData=RDataToProcess[k])
#                           master <- raster(masterToProcess[k])
#                           
#                           TEMP <- matrixToRaster(matrix=cell$output_gapFill$fill[,,j,i],
#                                                  raster=master)
#                           
#                           if(k %% 100 == 0){
#                             text <- paste0("Working on cell ", k)
#                             write(text, 
#                                   file = paste0( progressReportDir, "/mosaicking_progress.txt"),
#                                   append = TRUE)
#                           }
#                           
#                           return(TEMP)
#                         }
#       stopCluster(cluster)
# 
#       message("Mosaicking continues...")
#       
#       write("-------------------------",
#             paste0(progressReportDir, "/mosaicking_progress.txt"), 
#             append = TRUE)
#       TXT <- "Mosaicking continues..."
#       write(TXT,
#             file = paste0(progressReportDir, "/mosaicking_progress.txt"), 
#             append = TRUE)
#       write("-------------------------",
#             paste0(progressReportDir, "/mosaicking_progress.txt"), 
#             append = TRUE)
#       
#       tempMosaic <- mosaicking(m=MOSAIC)
# 
#       save_mosaic(mosaic=tempMosaic, 
#                   originalRaster=original, 
#                   outputDir = outputDir,
#                   dataType = dataType)
#     }
#   }
#   
#   write("===============================",
#         paste0( progressReportDir, "/mosaicking_progress.txt"), 
#         append = TRUE)
#   write(paste0("Ended at: ", as.character(Sys.time()[1])),
#         file = paste0( progressReportDir, "/mosaicking_progress.txt"), 
#         append = TRUE)
#   write("===============================",
#         paste0( progressReportDir, "/mosaicking_progress.txt"), 
#         append = TRUE)
#   
#   outputMessage <- paste0( "See ", textColor(text=outputDir, color = "red") )
#   message(outputMessage)
# }
# 
# mosaicking <- function(m){
#   m$fun <- max
#   do.call(what=mosaic, args=m)
# }
# 
# save_mosaic <- function(mosaic, originalRaster, outputDir, dataType="FLT4S"){
#   writeRaster(mosaic,
#               paste0(outputDir, "/", names(originalRaster), "_gapfill.tif"),
#               format = "GTiff", overwrite = TRUE, datatype = dataType)
# }

# --- Added on November 13, 2023
# --- Taken from original version @series_tiempo machine
# --- Secondary copy is @HUAWEI machine

# --- Added on Sept 19, 2023

getMosaicList <- function(i,j,imagesToProcess,
                          RDataToProcess, masterToProcess,
                          progressReportDir,numCores){
  
  originalImage <- raster(imagesToProcess[sqrt(length(imagesToProcess)) * (i-1) + j])
  logFILE <- paste0( progressReportDir, "/mosaicList_progress_",
                     names(originalImage), ".txt" )
  
  message("Working on file ", names(originalImage))
  detailsTEXT <- paste0("More details in ", textColor(text=logFILE, color = "red"))
  message(detailsTEXT)
  
  write("===============================",
        file = logFILE,
        append = TRUE)
  write(paste0("Started at: ", as.character(Sys.time()[1])),
        file = logFILE,
        append = TRUE)
  write("===============================",
        file = logFILE,
        append = TRUE)
  
  
  write("-------------------------",
        file = logFILE,
        append = TRUE)
  write(paste0("Working on file ", names(originalImage)),
        file = logFILE,
        append = TRUE)
  write("-------------------------",
        file = logFILE,
        append = TRUE)
  
  cluster <- parallel::makeCluster(numCores, outfile = "/dev/null")
  registerDoParallel(cluster)
  
  rasterList <- foreach(k = 1:length(RDataToProcess),
                        .export = c("LoadToEnvironment"),
                        .packages = c("raster","geoTS"))%dopar%{
                          
                          cell <- LoadToEnvironment(RData=RDataToProcess[k])
                          master <- raster(masterToProcess[k])
                          
                          master[is.na(master)] <- 1L
                          
                          TEMP <- matrixToRaster(matrix=cell$output_gapFill$fill[,,j,i],
                                                 raster=master)
                          
                          if(k %% 100 == 0){
                            text <- paste0("Working on cell ", k)
                            write(text,
                                  file = logFILE,
                                  append = TRUE)
                          }
                          
                          return(TEMP)
                        }
  stopCluster(cluster)
  
  write("===============================",
        file = logFILE,
        append = TRUE)
  write(paste0("Ended at: ", as.character(Sys.time()[1])),
        file = logFILE,
        append = TRUE)
  write("===============================",
        file = logFILE,
        append = TRUE)
  
  rasterList
}

doMosaicking <- function(rlist, numCores=23,
                         originalRaster,
                         outputDir,
                         progressReportDir){
  
  # rlist=MOSAIC
  # numCores=23
  # originalRaster = imagesToProcess[2]
  # outputDir = paste0(getwd(), "/b_00_02_part2/gapfill/filled")
  # progressReportDir = paste0(getwd(), "/b_00_02_part2/gapfill/progressReports")
  
  # rlist=listRaster
  # numCores=numCores
  # originalRaster=imagesToProcess[totalListFILES*(i-1)+j]
  # outputDir=outputDir
  # progressReportDir=progressReportDir
  
  message("Mosaicking continues ...")
  original <- raster(originalRaster)
  
  dir.create(paste0( outputDir, "/temp_", names(original) ))
  # progressReportDirTemp <- paste0( progressReportDir, "/temp_", names(original) )
  # dir.create(progressReportDirTemp)
  dirOutput <- paste0( outputDir, "/temp_", names(original) )
  detailsTEXT <- paste0("More details in ",
                        textColor(text=dirOutput, color = "red"))
  message(detailsTEXT)
  
  if(length(rlist) > 1) {
    res <- length(rlist) %% numCores
    blocks <- seq(0, length(rlist),
                  by = (length(rlist)-res) / numCores)
    leftCell <- blocks + 1
    rightCell <- c(blocks[-1], length(rlist))
    
    cluster <- parallel::makeCluster(numCores, outfile = "/dev/null")
    registerDoParallel(cluster)
    # r <- MOSAIC[1:(2*4558)]
    done <- foreach(k = isplitVector(x=rlist, chunkSize=(length(rlist)-res) / numCores),
                    i=icount(), #1:length(leftCell),
                    .export = "cellsMosaicking",
                    .packages = "raster") %dopar% {
                      cellsMosaicking(rasterList = k,
                                      cellNames = c(leftCell[i], rightCell[i]),
                                      dataType = dataType(original),
                                      saveName = paste0(dirOutput, "/",
                                                        names(original)),
                                      progressReportDir = progressReportDirTemp)
                    }
    
    stopCluster(cluster)
    
    cellsTIF <- mixedsort(list.files(path = dirOutput,
                                     pattern = ".tif",
                                     full.names = TRUE))
    
    message(paste0("Last step to mosaic ", names(original)))
    finalMOSAIC <- raster(cellsTIF[1])
    for(j in 2:length(cellsTIF)){
      AUX <- raster(cellsTIF[j])
      finalMOSAIC <- mosaic(x=finalMOSAIC, y = AUX,
                            fun=mean)
    }
    
  } else {
    finalMOSAIC <- rlist[[1]] * 1e4
  }
  
  message(paste0("Saving ", names(original)))
  writeRaster(finalMOSAIC,
              filename = paste0(outputDir, "/", names(original)),
              format = "GTiff",
              datatype= dataType(original),
              overwrite = TRUE)
  
}

cellsMosaicking <- function(rasterList, cellNames,
                            dataType, saveName,
                            progressReportDir){
  
  # rasterList <- MOSAIC[995:999]
  
  fileNAME <- paste0( progressReportDir, "/cellsMosaicking_cells_",
                      cellNames[1], "_", cellNames[2], "_progress.txt")
  
  write("===============================",
        file = fileNAME,
        append = TRUE)
  write(paste0("Started at: ", as.character(Sys.time()[1])),
        file = fileNAME,
        append = TRUE)
  write("===============================",
        file = fileNAME,
        append = TRUE)
  
  final_mosaic <- rasterList[[1]] * 1e4
  
  for(i in 2:length(rasterList)){
    
    if(i %% 100 == 0){
      text <- paste0("Working on cell ", i)
      write(text,
            file = fileNAME,
            append = TRUE)
    }
    
    aux <- rasterList[[i]] * 1e4
    final_mosaic <- mosaic(x=final_mosaic, y=aux, fun=mean)
  }
  
  write("-------------------------------",
        file=fileNAME,
        append = TRUE)
  TXT <- "   Saving partial mosaic..."
  write(TXT,
        file = fileNAME,
        append = TRUE)
  write("-------------------------------",
        file = fileNAME,
        append = TRUE)
  
  writeRaster(final_mosaic,
              filename = paste0(saveName, "_cells_",
                                cellNames[1], "_", cellNames[2]),
              format="GTiff",
              datatype=dataType,
              overwrite=TRUE)
  
  write("===============================",
        file = fileNAME,
        append = TRUE)
  write(paste0("Ended at: ", as.character(Sys.time()[1])),
        file = fileNAME,
        append = TRUE)
  write("===============================",
        file = fileNAME,
        append = TRUE)
}

# --- Added on November 7, 2023

parallel_mosaicking <- function(inputDirImages,
                                inputDirRData,
                                inputDirMaster,
                                outputDir,
                                scaleFactor=1e4,
                                dataType="INT4S",
                                progressReportDir,
                                numCores=23){
  
  # inputDirImages = paste0(getwd(), "/data/cell_165_gapfill/block1")
  # inputDirRData = DIRoutput
  # inputDirMaster = paste0( getwd(), "/data/cell_165_gapfill/block1/master")
  # outputDir = paste0(getwd(), "/data/cell_165_gapfill/block1/gapfill/filled")
  # progressReportDir = DIRprogress
  # numCores = 2
  
  
  initTEXT <- paste0("Loading arguments: ", textColor("original TIF files,", color="yellow"),
                     textColor(" RData files,", color="red"), textColor(" master files, ", color = "blue"),
                     textColor(" etc.", color = "green"))
  
  message(initTEXT)
  
  imagesToProcess <- mixedsort(list.files(path=inputDirImages,
                                          pattern = ".tif",
                                          full.names = TRUE))
  
  RDataToProcess <- mixedsort(list.files(path = inputDirRData,
                                         pattern = ".RData",
                                         full.names = TRUE))
  
  masterToProcess <- mixedsort(list.files(path = inputDirMaster,
                                          pattern = ".tif",
                                          full.names = TRUE))
  
  totalListFILES <- sqrt(length(imagesToProcess))
  
  for(i in 1:totalListFILES){
    for(j in 1:totalListFILES){
      listRaster <- getMosaicList(i=i,j=j,
                                  imagesToProcess = imagesToProcess,
                                  RDataToProcess = RDataToProcess,
                                  masterToProcess = masterToProcess,
                                  progressReportDir = progressReportDir,
                                  numCores = numCores)
      
      doMosaicking(rlist=listRaster, numCores=numCores,
                   originalRaster=imagesToProcess[totalListFILES*(i-1)+j],
                   outputDir=outputDir,
                   progressReportDir=progressReportDir, scaleFactor=scaleFactor,
                   dataType=dataType)
    }
  }
  
  outputMessage <- paste0( "See ", textColor(text=outputDir, color = "red") )
  message(outputMessage)
}











