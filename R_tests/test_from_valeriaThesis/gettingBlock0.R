
library(igapfill)
source( paste0( getwd(), "/R/get_minBlock.R" ) )

# igapfill() # como no hay q hacer recortes, es mejor emplear directamente
# el codigo de abajo

# ---

DIR <- "C:/Users/inder/OneDrive/Desktop/Rpkgs_repo/igapfill/data"

YEARS <- 2017:2018

# create_dirs(path=DIR,
#             startYear = YEARS[1],
#             endYear = YEARS[2])

allDIRS <- list.dirs(path=DIR,
                     full.names = TRUE)[-1]

# ---

allDIRS


# --- getting the "splits" RIGHT

SPLITs <- list.files(path=DIR,
                     pattern=".tif",
                     full.names=TRUE)

STACK <- stack(SPLITs)

raster::writeRaster(x=subset(STACK,1:2),
            filename = paste0( allDIRS[7], "/", YEARS[1] ),
            format="GTiff",
            dataType="INT2S",
            overwrite=TRUE)

raster::writeRaster(x=subset(STACK,3:4),
                    filename = paste0( allDIRS[8], "/", YEARS[2] ),
                    format="GTiff",
                    dataType="INT2S",
                    overwrite=TRUE)

# ---

MASTER <- getMASTER(path=DIR, outputPath = allDIRS[3])

# LAT <- get_LAT(MASTER) # regresa un renglon menos, TODO
LON <- get_LON(MASTER)

LAT <- as.character(seq(ymin(MASTER), 
                        ymax(MASTER), 
                        by = min(res(MASTER))))[-1]

# ---

applyGapfill(
  inputDir=allDIRS[6],
  outputDir=allDIRS[4],
  progressDir=allDIRS[5],
  lat=LAT,
  lon=LON,
  days=2:3, # puedes cambiar los valores de este argumento
  years=YEARS,
  numCores=4,
  scale=1e-4
)

 