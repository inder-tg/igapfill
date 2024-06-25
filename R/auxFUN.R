# --- Setting color of console text
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

print_years_array <- function(p, years){
  what_to_print <- sapply(1:p^2, function(s) 
    paste0("(", s, ")"))
  
  for(k in 1:p){
    cat(years[k],
        paste0("STACK_", k, ":"),
        what_to_print[1:p + (k-1)*p], "\n")
  }
}

# @param listLayer list, containing lists with names of files to be assembled as a 4D array.
# @param       lon character, vector whose entries indicate longitude coordinates.
# @param       lat character, vector whose entries indicate latitude coordinates. 
# @param      days numeric, vector indicating what DoYs are being considered. Length of this
#                  object must be equal to length of \code{listLayer}. 
# @param     years integer, vector indicating what years are being considered. Length of this
#                  object must be equal to length of \code{listLayer}.
# 
# @return An array of 4 dimensions: longitud, latitude, days and years
# 
# @seealso \code{get_LON}, \code{get_LAT}, \code{getListFiles}
# 
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

# ---

get_applyGapfill <- function(inputDir, outputDir, progressDir, 
                             lat, lon, days, years, 
                             numCores=6, scale=1e-4, 
                             clipRange=c(-1,1),
                             addArgToReport=TRUE){
  
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
  
  cluster <- parallel::makeCluster(numCores, outfile="/dev/null")
  registerDoParallel(cluster)
  name_RData_output <- paste0( outputDir, "/gapfill_output_cell_" )
  
  logFILE <- paste0(progressDir, "/gapfill_progress.txt")
  
  write("===============================",
        file = logFILE, append = TRUE)
  write(paste0("Started at: ", as.character(Sys.time()[1])),
        file = logFILE, append = TRUE)
  write("===============================",
        file = logFILE, append = TRUE)
  
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
                         write(text, file = logFILE, append = TRUE)
                       }
                     }
  stopCluster(cluster)
  write("===============================",
        file = logFILE, append = TRUE)
  write(paste0("Ended at: ", as.character(Sys.time()[1])),
        file = logFILE, append = TRUE)
  write("===============================",
        file = logFILE, append = TRUE)
  
  if(addArgToReport){
    write("==========",
          file = logFILE, append = TRUE)
    TEXT <- "Arguments: "
    write(TEXT,
          file = logFILE, append = TRUE)
    write("==========",
          file = logFILE, append = TRUE)
    
    write(paste0("Input directory: ", inputDir),
          file = logFILE, append = TRUE)
    write(paste0("Output directory: ", outputDir),
          file = logFILE, append = TRUE)
    write(paste0("Progress Report directory: ", progressDir),
          file = logFILE, append = TRUE)
    write(paste0("lat (as vector): first & last entries: ", lat[1], 
                 ", ", lat[length(lat)], 
                 "; length equal to ", length(lat)),
          file = logFILE, append = TRUE)
    write(paste0("lon (as vector): first & last entries: ", lon[1], 
                 ", ", lon[length(lon)],
                 "; length equal to ", length(lon)),
          file = logFILE, append = TRUE)
    write(paste0("days (as vector, only first day of each year): first entry ", days[1], 
                 ", last entry ", days[length(days)],
                 ", length equal to ", length(days)),
          file = logFILE, append = TRUE)
    write(paste0("years: ", years[1], ":", years[length(years)]),
          file = logFILE, append = TRUE)
    write(paste0("Number of cores in used: ", numCores),
          file = logFILE, append = TRUE)
    write(paste0("Scale factor: ", scale),
          file = logFILE, append = TRUE)
    write(paste0("Clip range: (", clipRange[1], ",", clipRange[2], ")"),
          file = logFILE, append = TRUE)
  }
  
  outputMessage <- paste0( "See ", textColor(text=outputDir, color = "red") )
  message(outputMessage)
}

# ---

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  env
}

# globalVariables('k')
# 
# mosaicking <- function(m){
#   m$fun <- max
#   do.call(what=mosaic, args=m)
# }
# 
# save_mosaic <- function(mosaic, originalRaster, outputDir, 
#                         dataType="FLT4S"){
#   NAMES <- names(originalRaster)
#   writeRaster(x=mosaic,
#               filename=paste0(outputDir, "/", NAMES, "_gapfill.tif"),
#               format = "GTiff", overwrite = TRUE, 
#               datatype = dataType)
# }

# --- Added on Aug 29, 2023

getAggregate_terra <- function(spRaster, h = 1, v = 1){
  
  if( terra::nlyr(spRaster) > 1 ){
    spRaster <- terra::subset(spRaster,1)
  }
  
  agg <- terra::aggregate(x=spRaster, 
                          fact=c(v,h))
  
  agg
}

# ---

globalVariables("j")
parallel_crop <- function(raster, numCores, polygon_extent){

  closter <- parallel::makeCluster(spec = numCores,
                                   outfile = "")
  registerDoParallel(closter)

  output <- foreach(j = 1:nlyr(raster), .packages = c("terra")) %dopar% {
    s <- terra::crop(raster[[j]], polygon_extent)
    return(s)
  }

  stopCluster(closter)

  output
}

# ---

split_replace_terra <- function(raster, partPerSide, h, v, outputPath, 
                                name, save = TRUE, replace = FALSE, 
                                valToReplace, replacedBy, 
                                fileType = "GTiff", dataType,
                                parallelProcessing = FALSE, 
                                numCores = 20, cellsToProcess, ...){
  
  if(missing(raster)){
    stop("raster must be provided")
  }
  
  if(missing(outputPath)){
    stop("outputPath must be provided")
  }
  
  if(missing(name)){
    stop("name must be provided")
  }
  
  if(replace){
    if(missing(valToReplace) | missing(replacedBy)){
      stop("When replace = TRUE, valToReplace and replacedBy must be specified")
    }
  }
  
  if(missing(partPerSide)){
    if(missing(h) | missing(v)){
      stop("h and v must be provided")
    } else {
      if(missing(cellsToProcess)){
        cellsToProcess <- 1:( terra::ncell( raster ) /(h*v) )
      }
    }
  } else {
    h <- ceiling(ncol(raster)/partPerSide)
    
    v <- ceiling(nrow(raster)/partPerSide)
    
    if(missing(cellsToProcess)){
      cellsToProcess <- 1:(partPerSide^2)
    }
  }
  
  if(missing(dataType)){
    dataType <- datatype(raster)[1]
  }
  
  agg <- getAggregate_terra(spRaster = raster, 
                            h = h, v = v)
  
  agg[] <- 1:ncell(agg)
  
  agg_poly <- as.polygons(agg) #rasterToPolygons(agg)
  
  names(agg_poly) <- "polis"
  
  message(paste0("Started at: ", as.character(Sys.time()[1])))
  
  pb <- txtProgressBar(min = 0, max = length(cellsToProcess), style = 3)
  for(i in cellsToProcess){
    Sys.sleep(0.1)
    
    extent_polygon <- ext(agg_poly[agg_poly$polis == i,])
    
    # --- create temp dir
    dir.create(path = paste0(outputPath, "/temp_", name, "_", i),
               showWarnings = F)
    rasterOptions(tmpdir = paste0(outputPath, "/temp_", name, "_", i))
    # ---
    
    temp_r <- rast()  
    
    if( parallelProcessing ){
      
      output <- parallel_crop(raster = raster, numCores = numCores, 
                              polygon_extent = extent_polygon)
      
      for(k in 1:nlyr(raster)){
        add(temp_r) <- output[[k]]
        # temp_r <- addLayer(temp_r, )
      }
      
    } else {
      for(k in 1:nlyr(raster)){
        aux_r <- terra::crop(raster[[k]], extent_polygon)
        add(temp_r) <- aux_r
      }
    }
    
    # if(replace == TRUE){
    #   temp_r <- reclassify( temp_r, cbind( valToReplace, replacedBy ) )
    # }
    
    if(save){
      writeRaster(x=temp_r, 
                  filename = paste0(outputPath, "/", name, "_", i, ".tif"),
                  filetype = fileType, datatype = dataType, 
                  overwrite = TRUE, ...)  
    }
    
    unlink(paste0(outputPath, "/temp_", name, "_", i), recursive = TRUE)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  message(paste0("Finished at: ", as.character(Sys.time()[1])))
  
}

# --- Added on November 13, 2023

getMosaicList <- function(i,j,imagesToProcess,
                          RDataToProcess, masterToProcess,
                          progressReportDir, numCores){
  
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

globalVariables('k')

globalVariables('i')

doMosaicking <- function(rlist, numCores=23,
                         originalRaster,
                         outputDir,
                         progressReportDir,
                         scaleFactor=1e+4,
                         dataType){
  
  message("Mosaicking continues ...")
  original <- raster(originalRaster)
  
  dir.create(paste0( outputDir, "/temp_", names(original) ))
  progressReportDirTemp <- paste0( progressReportDir, "/temp_", names(original) )
  dir.create(progressReportDirTemp)
  dirOutput <- paste0( outputDir, "/temp_", names(original) )
  # detailsTEXT <- paste0("More details in ",
  #                       textColor(text=dirOutput, color = "red"))
  detailsTEXT <- paste0("More details in ",
                        textColor(text=progressReportDir, color = "red"))
  message(detailsTEXT)
  
  if(length(rlist) > 1) {
    res <- length(rlist) %% numCores
    blocks <- seq(0, length(rlist),
                  by = (length(rlist)-res) / numCores)
    leftCell <- blocks + 1
    rightCell <- c(blocks[-1], length(rlist))
    
    cluster <- parallel::makeCluster(numCores, outfile = "/dev/null")
    registerDoParallel(cluster)
    done <- foreach(k=isplitVector(x=rlist, chunkSize=(length(rlist)-res) / numCores),
                    i=icount(), #1:length(leftCell),
                    .export = "cellsMosaicking",
                    .packages = "raster") %dopar% {
                      cellsMosaicking(rasterList = k,
                                      cellNames = c(leftCell[i], rightCell[i]),
                                      # dataType = dataType(original),
                                      dataType = dataType,
                                      saveName = paste0(dirOutput, "/",
                                                        names(original)),
                                      scaleFactor=scaleFactor,
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
    finalMOSAIC <- rlist[[1]] * scaleFactor
  }
  
  message(paste0("Saving ", names(original)))
  writeRaster(finalMOSAIC,
              filename = paste0(outputDir, "/", names(original)),
              format = "GTiff",
              datatype= dataType(original),
              overwrite = TRUE)
  
}

cellsMosaicking <- function(rasterList, cellNames,
                            dataType, saveName, scaleFactor=1e4,
                            progressReportDir){
  
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
  
  final_mosaic <- rasterList[[1]] * scaleFactor
  
  for(i in 2:length(rasterList)){
    
    if(i %% 100 == 0){
      text <- paste0("Working on cell ", i)
      write(text,
            file = fileNAME,
            append = TRUE)
    }
    
    aux <- rasterList[[i]] * scaleFactor
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





