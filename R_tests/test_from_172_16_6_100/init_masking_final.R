
# --- June 25, 2024
# --- TAKEN from heatIsland project written @172.16.6.100 machine
# --- SIMPLY takes original images (from /blockNUMBER) and gapfilled images (from /blockNUMBER/gapfill/filled)
# --- and plot them all. Thus, user can have a sort of BEFORE and AFTER applying igapfill().


library(raster)

dirBLOCK <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill/block22"
dirOUTPUT <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill/block22/gapfill/filled"

initFILES <- list.files(path = dirBLOCK, 
                        pattern = ".tif",
                        full.names = TRUE)

finalFILES <- list.files(path = dirOUTPUT,
                        pattern = ".tif",
                        full.names = TRUE)

initSTACK <- stack(initFILES)
finalSTACK <- stack(finalFILES)

initSTACK
finalSTACK

# plot(initSTACK)
# plot(finalSTACK)

# dirMASK <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill/block1_masked"
# 
# for(i in 1:nlayers(initSTACK)){
#   NAME <- strsplit(initFILES[i], "/")[[1]][8]
#   
#   initTEMP <- subset(initSTACK, i)
#   finalTEMP <- subset(finalSTACK, i)
#   finalTEMP <- mask(finalTEMP, initTEMP)
#   writeRaster(x=finalTEMP,
#               filename = paste0(dirMASK, "/", NAME),
#               datatype=dataType(initTEMP))
# }

