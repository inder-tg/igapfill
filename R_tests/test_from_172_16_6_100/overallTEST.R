
library(igapfill)
library(geoTS)
library(doParallel)
library(raster)

DIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill/block23_test"

YEARS <- 2018:2019

create_dirs(path=DIR,
            startYear = YEARS[1],
            endYear = YEARS[2])

allDIRS <- list.dirs(path=DIR,
                     full.names = TRUE)[-1]

dimsReport(path=DIR,
           message = "test")

listFILES <- list.files(path = DIR,
                        pattern = ".tif",
                        full.names = TRUE)

stackLIST <- list()

stackLIST[[1]] <- stack(listFILES[1:2])
stackLIST[[2]] <- stack(listFILES[3:4])

MASTER <- raster(listFILES[1])
MASTER[is.na(MASTER)]<- 1L

stackLIST[[3]] <- MASTER

waysToSplit(h=45,v=40,raster=MASTER)

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
              "INT4S")
OUTPUTPATH <- c(allDIRS[7], allDIRS[8], 
                allDIRS[3])

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
                        h=50, v=39,
                        outputPath = TEMP$OUTPUTpath,
                        name=TEMP$NAME,
                        dataType=TEMP$DataType)
        }

stopCluster(closter)

# ---
sampleSPLIT <- list.files(path=allDIRS[3],
                          pattern=".tif",
                          full.names=TRUE)

MASTER_cell1 <- raster(sampleSPLIT[1])
LAT <- get_LAT(MASTER_cell1)
LON <- get_LON(MASTER_cell1)

# YEARS <- 2002:2003

applyGapfill(
  inputDir=allDIRS[6],
  outputDir=allDIRS[4],
  progressDir=allDIRS[5],
  lat=LAT,
  lon=LON,
  days=2:3,
  years=YEARS,
  numCores=23,
  scale=1e0
)

# ---

parallel_mosaic(inputDirImages = DIR,
                inputDirRData = allDIRS[4],
                inputDirMaster = allDIRS[3],
                outputDir = allDIRS[2],
                progressReportDir = allDIRS[5],
                scaleFactor = 1e0,
                dataType = "FLT4S",
                numCores = 23)











