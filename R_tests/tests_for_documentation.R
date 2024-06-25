library(igapfill)
library(geoTS)
library(doParallel)
library(raster)

# --- only real test in package

DIR_extdata <- system.file("extdata", package = "igapfill")

DIR <- paste0(DIR_extdata, "/test")

dir.create(DIR)

YEARS <- 2022:2023

create_dirs(path = DIR,
            startYear = YEARS[1],
            endYear = YEARS[2])

unlink(x=DIR, recursive = TRUE)

# ---

origFILE <- list.files(path = DIR_extdata,
                        pattern = ".tif",
                        full.names = TRUE)

mohinora <- rast(origFILE)

mohinoraNAMES <- names(mohinora)

for(i in 1:nlyr(mohinora)){
  writeRaster(x=subset(mohinora, i),
              filename=paste0(DIR, "/", mohinoraNAMES[i]),
              datatype="INT2S",
              overwrite=TRUE)
}

allDIRS <- list.dirs(path=DIR, full.names = TRUE)

# dimsReport(path=DIR, message = "Images in this path have:")

dimsReport(path=DIR)

# ---

listFILES <- list.files(path = allDIRS[1], pattern = ".tif",
                       full.names = TRUE)

stackLIST <- list()

stackLIST[[1]] <- stack(listFILES[1:2])
stackLIST[[2]] <- stack(listFILES[3:4])

MASTER <- raster(listFILES[1])
MASTER[is.na(MASTER)]<- 1L

stackLIST[[3]] <- MASTER

waysToSplit(h=11,v=8,raster=MASTER)

# ---

splitReplaceParams <- function(stack, name, datatype, 
                               outputpath, k){
  TEMPstack <- stack[[k]]
  TEMPname <- name[k]
  TEMPdatatype <- datatype[k]
  TEMPoutputPath <- outputpath[k]
  
  list(STACK=TEMPstack, NAME=TEMPname, 
       DataType=TEMPdatatype, OUTPUTpath=TEMPoutputPath)
}

NAME <- c(YEARS[1], YEARS[2], "master")
DATATYPE <- c(dataType(stackLIST[[1]])[1],
              dataType(stackLIST[[2]])[1],
              "INT2S")
OUTPUTPATH <- c(allDIRS[8], allDIRS[9], allDIRS[4])

closter <- parallel::makeCluster(spec = 3,
                                 outfile = "")
registerDoParallel(closter)

foreach(i=1:length(stackLIST),
        .packages=c("geoTS")) %dopar% {
          TEMP <- splitReplaceParams(stack = stackLIST,
                                     name = NAME,
                                     datatype = DATATYPE,
                                     outputpath = OUTPUTPATH,
                                     k = i)
          split_replace(raster=TEMP$STACK,
                        h=8, v=7,
                        outputPath = TEMP$OUTPUTpath,
                        name=TEMP$NAME,
                        dataType=TEMP$DataType)
        }

stopCluster(closter)

# ---

sampleSPLIT <- list.files(path=allDIRS[4],
                          pattern=".tif",
                          full.names=TRUE)

MASTER_cell1 <- raster(sampleSPLIT[1])
LAT <- get_LAT(MASTER_cell1)
LON <- get_LON(MASTER_cell1)

LAT <- as.character(seq(ymin(MASTER_cell1), 
                 ymax(MASTER_cell1), 
                 by = min(res(MASTER_cell1))))[-1]

as.character(seq(xmin(MASTER_cell1), 
                 xmax(MASTER_cell1), 
                 by = min(res(MASTER_cell1))))[-1]

as.character(seq(xmin(MASTER_cell1), 
                 xmax(MASTER_cell1), 
                 by = res(MASTER_cell1)[1]))[-1]


applyGapfill(
  inputDir=allDIRS[7],
  outputDir=allDIRS[5],
  progressDir=allDIRS[6],
  lat=LAT,
  lon=LON,
  days=2:3,
  years=YEARS,
  numCores=5
)


# ---

parallel_mosaic(inputDirImages = DIR,
                inputDirRData = allDIRS[5],
                inputDirMaster = allDIRS[4],
                outputDir = allDIRS[3],
                progressReportDir = allDIRS[6],
                dataType = "INT2S",
                numCores = 6)

unlink(x=DIR_extdata, recursive = TRUE)

# ---

library(gtools)

splits_test <- mixedsort(list.files(path=allDIRS[9],
                         pattern = ".tif",
                         full.names = TRUE))

r <- rast(splits_test[1])

plot(r)


final_output <- list.files(path="C:/Users/inder/AppData/Local/R/win-library/4.3/igapfill/extdata/tests/gapfill/filled",
                           pattern=".tif",
                           full.names=TRUE)

final_spatRaster <- rast(final_output[1:4])

plot(final_spatRaster)


init_input <- list.files(path=DIR,
                         pattern = ".tif",
                         full.names = TRUE)

init_spatRaster <- rast(init_input)

plot(init_spatRaster)

# gapfill_test <- mixedsort(list.files(path=allDIRS[5],
#                                      pattern = ".RData",
#                                      full.names = TRUE))
# 
# q <- LoadToEnvironment(gapfill_test[1])


