#' Console-based application for gap-filling Earth Observation datasets
#' 
#' Command-line user-friendly application that allows the application of 
#' \code{\link[gapfill]{Gapfill}} to a set of satellite images.
#' 
#' This function is a wrap-up of \code{\link[igapfill]{create_dirs}}, 
#' \code{\link[igapfill]{sort_split}} and \code{\link[igapfill]{applyGapfill}}
#' allowing users to provide some of the arguments employed by these functions.
#' 
#' @param saveArguments logical. Should the arguments defined during the execution of
#'                      this function be added to a progress report file? Default is \code{TRUE}.
#' 
#' @export
#' 
#' @importFrom raster stack
#' 
#' @seealso \code{\link[igapfill]{sort_split}}, \code{\link[parallel]{detectCores}},
#' \code{\link[igapfill]{applyGapfill}}
#' 
#' @return At the specified location there will be \code{.RData} files containing the output of 
#' parallel-based calls to \code{\link[gapfill]{Gapfill}}.
#' 
igapfill <- function(saveArguments=TRUE){
  
  PATH <- readline("Full path (directory containing images to process): ")
  
  if(!dir.exists(PATH)){
    stop(paste0(PATH, " does not exists"))
  }
  
  question_i <- readline("Have you set up this directory according with my documentation? [Y/n] ")
  
  if(question_i == "n" | question_i == "no"){
    question_ii <- readline("Do you want me to set up your working directory now? [Y/n] ")
    
    if(question_ii == "n" | question_ii == "no"){
      stop("Process stopped with no action taken")
    } else {
      START_YEAR <- as.numeric(readline("First year to process: "))
      END_YEAR <- as.numeric(readline("Last year to process: "))
      
      create_dirs(path=PATH, startYear = START_YEAR, endYear = END_YEAR)
      # inputDir <- paste0(PATH, "/splits")
      # outputDir <- paste0(PATH, "/output")
      # progressDir <- paste0(PATH, "/progressReports")
      # masterDir <- paste0(PATH, "/master")
    }
  }
  
  question_iii <- readline("Have you splitted up your original images? [Y/n] ")
  
  if(question_iii == "n" | question_iii == "no"){
    question_iv <- readline("Do you want me to take care of the splitting process? [Y/n] ")
    
    if(question_iv=="n" | question_iv == "no"){
      return("Process stopped with no action taken")
    } else {
      START_YEAR <- as.numeric(readline("First year to process: "))
      END_YEAR <- as.numeric(readline("Last year to process: "))
      PARALLEL <- as.character(readline("Should I do some parallel computing for this process? [Y/n] "))
      PARALLEL <- ifelse(PARALLEL=="n", FALSE, TRUE)
      
      if(PARALLEL) {
        NUMCORES_sort_split <- as.numeric(readline("How many CPU cores should I use? "))
      } else {
        NUMCORES_sort_split <- 1
      }
      
      sort_split(path = PATH, startYear = START_YEAR, 
                 endYear = END_YEAR,
                 parallelProcessing = PARALLEL, 
                 numCores = NUMCORES_sort_split)
    }
  } 
  
  dirInput <- paste0(PATH, "/gapfill/splits")
  
  dirOutput <- paste0(PATH, "/gapfill/output")
  
  dirProgress <- paste0(PATH, "/gapfill/progressReports")
  
  MSG <- "gapfill application starts here"
  message(textColor(MSG, "green"))
  
  START_YEAR <- as.numeric(readline("First year to process: "))
  
  END_YEAR <- as.numeric(readline("Last year to process: "))

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
  question_scale <- readline("Do you want to re-scale the observations in your images? [Y/n] ")
  
  # SCALE_question <- ifelse()
  
  if(question_scale=="no" | question_scale == "n"){
    SCALE <- 1
  } else {
    SCALE <- as.numeric(readline("Scale: "))
  }
  
  question_clip <- readline("Do you want to change the clip range? [Y/n] ")
  
  if(question_clip=="no" | question_clip=="n"){
    CLIP_RANGE <- c(-1,1)
  } else {
    CLIP_min <- as.numeric(readline("Lower limit: "))
    CLIP_max <- as.numeric(readline("Upper limit: "))
    CLIP_RANGE <- c(CLIP_min, CLIP_max)
  }

  COMMENT <- "gapfilling is on"
  message(textColor(COMMENT, "green"))
  
  applyGapfill(inputDir = dirInput,
               outputDir = dirOutput,
               progressDir = dirProgress,
               lat = LATITUDE, lon = LONGITUDE,
               days = DAYS, years = YEARS,
               numCores = NUMCORES,
               scale = SCALE, clipRange = CLIP_RANGE,
               addArgToReport = saveArguments)
  
  message("Rasterization/mosaicking is in order")
  # cat(textColor("nROW", "red"), "=", rowCELL, "\n")
  TXT <- "We suggest to use parallel_mosaic() to continue"
  message(textColor(TXT, "green"))
  
}
