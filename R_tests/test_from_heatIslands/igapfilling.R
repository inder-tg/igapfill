
# --- June 25, 2024

# --- ORIGINALLY taken from heatIslands R project
# --- CODE below shows how to apply igapfill in order to fill gaps in images from 
# --- each BLOCK.
# --- SOME functions may have changed their names, for instance, I belive that
# --- parallel_mosaicking() below is now called paralle_mosaic(). PLEASE check it out
# --- carefully

# --- SETTING a BLOCK

# --- block15

DIRS <- list.dirs(path=paste0(getwd(), "/data/cell_165_original"),
                  full.names = TRUE)

DIRS <- DIRS[-1]

files2002 <- mixedsort(list.files(path = DIRS[3],
                                  pattern = ".tif",
                                  full.names = TRUE))
files2003 <- mixedsort(list.files(path = DIRS[4],
                                  pattern = ".tif",
                                  full.names = TRUE))

r1 <- raster(files2002[5])
r2 <- raster(files2002[6])

BLOCK <- 15

writeRaster(r1,
            filename = paste0(getwd(), "/data/cell_165_gapfill/block", 
                              BLOCK, 
                              "/NDVI_2002_4"),
            format="GTiff", datatype=dataType(r1),
            overwrite=TRUE)


writeRaster(r2,
            filename = paste0(getwd(), "/data/cell_165_gapfill/block", 
                              BLOCK, 
                              "/NDVI_2002_5"),
            format="GTiff", datatype=dataType(r2),
            overwrite=TRUE)

r3 <- raster(files2003[5])
r4 <- raster(files2003[6])

writeRaster(r3,
            filename = paste0(getwd(), "/data/cell_165_gapfill/block", 
                              BLOCK, 
                              "/NDVI_2003_4"),
            format="GTiff", datatype=dataType(r3),
            overwrite=TRUE)

writeRaster(r4,
            filename = paste0(getwd(), "/data/cell_165_gapfill/block", BLOCK, 
                              "/NDVI_2003_5"),
            format="GTiff", datatype=dataType(r4),
            overwrite=TRUE)

# --- APPLYING igapfill to BLOCK

igapfill() # initial, create tree folders, ends right before copying files to splits

BLOCK <- 15

DIRinput <- paste0("C:/Users/inder/OneDrive/Desktop/heatIslands/data/cell_165_gapfill/block", 
                   BLOCK, 
                   "/gapfill/splits")
DIRoutput <- paste0("C:/Users/inder/OneDrive/Desktop/heatIslands/data/cell_165_gapfill/block", 
                    BLOCK, 
                    "/gapfill/output")
DIRprogress <- paste0("C:/Users/inder/OneDrive/Desktop/heatIslands/data/cell_165_gapfill/block",
BLOCK, "/gapfill/progressReports")

fileLIST <- mixedsort(list.files(path=paste0(getwd(), 
                                             "/data/cell_165_gapfill/block",
                                             BLOCK),
                       pattern = ".tif",
                       full.names = TRUE))

STACK <- stack(fileLIST)

plot(STACK)

stack2002  <- stack(fileLIST[1:2])
# names(stack2020) <- c("NDVI_2016_2", "NDVI_2016_3")
stack2003  <- stack(fileLIST[3:4])
# names(stack2018) <- c("NDVI_2017_2", "NDVI_2017_3")

writeRaster(stack2002,
            filename = paste0( getwd(), "/data/cell_165_gapfill/block", 
                               BLOCK, 
                               "/gapfill/splits/2002/stack2002" ),
            format="GTiff", datatype=dataType(STACK)[1], overwrite=TRUE)

writeRaster(stack2003,
            filename = paste0( getwd(), "/data/cell_165_gapfill/block", 
                               BLOCK, 
                               "/gapfill/splits/2003/stack2003" ),
            format="GTiff", datatype=dataType(STACK)[1], overwrite=TRUE)

igapfill()

# applyGapfill(inputDir = DIRinput,
#              outputDir = DIRoutput,
#              progressDir = DIRprogress,
#              lat=LAT,
#              lon=LON,
#              days=DAYS,
#              years=YEARS)
# library(sta)

test <- subset(STACK,1)

MASTER <- getMaster(test)

writeRaster(MASTER,
            filename = paste0( getwd(), "/data/cell_165_gapfill/block", 
                               BLOCK, 
                               "/master/master"),
            format="GTiff", 
            datatype=dataType(test), overwrite=TRUE)

source( paste0( getwd(), "/Rscripts/auxFUN.R" ) )

dirIMAGES <- paste0(getwd(), "/data/cell_165_gapfill/block", BLOCK)
dirMASTER <- paste0(getwd(), "/data/cell_165_gapfill/block", BLOCK, 
                     "/master")
dirOUTPUT <- paste0(getwd(), "/data/cell_165_gapfill/block", BLOCK, 
                    "/gapfill/filled")

parallel_mosaicking(
  inputDirImages = dirIMAGES,
  inputDirRData = DIRoutput,
  inputDirMaster = dirMASTER,
  outputDir = dirOUTPUT,
  progressReportDir = DIRprogress,
  numCores = 2
)

testLIST <- mixedsort(list.files(path=paste0( getwd(), "/data/cell_165_gapfill/block", 
                                              BLOCK, 
                                              "/gapfill/filled"),
                       pattern = ".tif",
                       full.names = TRUE))

testSTACK <- stack(testLIST)

plot(STACK)
plot(testSTACK)
