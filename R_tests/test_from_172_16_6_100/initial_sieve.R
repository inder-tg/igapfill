
# --- Created on November 13, 2023
# --- testing igapfill_0.0.13 on MTY dataset 

library(igapfill)
library(gtools)
library(heatmaply)

# --- let's check data availability on MTY NDVI dataset

missVal_sieve <- function(DIR, amountFiles, totalPixels, startYear, endYear){
  percentMat <- matrix(nrow=length(DIR), ncol=amountFiles)
  totalPixels <- totalPixels #36 * 36
  
  for(i in seq_len(nrow(percentMat))){
    TIFs <- mixedsort(list.files(path = DIR[i], full.names = TRUE))
    for(j in seq_len(ncol(percentMat))){
      r <- rast(TIFs[j])
      percentMat[i,j] <- as.numeric(global(r, fun="isNA")) / totalPixels
    }
  }
  
  row.names(percentMat) <- startYear:endYear # 2000:2022
  colnames(percentMat) <- month.name
  
  # ---
  
  percentMat  
}

# ---

testDIR <- "/home/itecuapetla/Desktop/heatIsland/data_copy"
dataDIRS <- list.dirs(path=testDIR)
dataDIRS <- dataDIRS[-1]

# sampleTIFfiles <- mixedsort(list.files(path = dataDIRS[1],
#                                        pattern = ".tif",
#                                        full.names = TRUE))
# 
# r <- rast(sampleTIFfiles[1])
# 
# r

original_sieve <- missVal_sieve(DIR=dataDIRS,
                                amountFiles = 12,
                                totalPixels = 1560 * 2250,
                                startYear = 2000,
                                endYear = 2022)

heatmaply(original_sieve,
          limits=c(0,1),
          colors = cool_warm,
          dendrogram="none",
          xlab="", ylab="",
          main="% Missing Data",
          scale = "none",
          draw_cellnote = TRUE,
          margins=c(60,100,40,20),
          grid_color="white",
          grid_width=0.00001,
          titleX = FALSE,
          hide_colorbar = TRUE,
          labCol = colnames(original_sieve),
          labRow = rownames(original_sieve),
          heatmap_layers = theme(axis.line=element_blank())
)

# --- masking product to have 2250 cols

listFILES <- mixedsort(list.files(path=blockDIRS[1],
                                  pattern = ".tif",
                                  full.names = TRUE) )

r <- rast(listFILES[1])

r1 <- rast(nrows=1560, ncol=2250, 
           crs=crs(r), 
           xmin=xmin(r),
           xmax=xmin(r)+67500,
           ymin=ymin(r),
           ymax=ymin(r)+46800,
           resolution=res(r))

r1[] <- 1

r2 <- crop(r,r1)

library(mapview)

mr <- mapview(r, maxpixels=3538572)
# mr1 <- mapview(r1, maxpixels=3538572)
mr2 <- mapview(r2, maxpixels=3538572)

mr+mr2

writeRaster(r2, 
            filename = paste0( "/home/itecuapetla/Desktop/heatIsland", 
                               "/small_mask.tif" ))

# --- crop to original NDVI, generated a copy folder

heatIsland_DIR <- "/home/itecuapetla/Desktop/heatIsland"
mask_small <- raster(paste0( "/home/itecuapetla/Desktop/heatIsland", 
                             "/small_mask.tif" ))

YEARS <- 2000:2022
for(i in 1:length(YEARS)){
  listFILES <- mixedsort(list.files(path=dataDIRS[i],
                                    pattern = ".tif",
                                    full.names = TRUE) )
  
  dirNAME <- paste0( heatIsland_DIR, "/data_copy/", YEARS[i] )
  dir.create(path = dirNAME)
  for(j in 1:length(listFILES)){
    TEMP <- raster(listFILES[j])    
    getName <- strsplit(listFILES[j], "/")[[1]][8]
    TEMP_crop <- crop(x=TEMP, y=mask_small)
    TEMP_mask <- mask(TEMP_crop, mask_small)
    writeRaster(TEMP_mask,
                filename = paste0( dirNAME, "/", getName ),
                format = "GTiff",
                datatype=dataType(TEMP),
                overwrite=TRUE)
  }  
}

# ---

