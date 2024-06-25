
library(igapfill)
library(gtools)

igapfill()

listFILES <- mixedsort( list.files( path = paste0(getwd(), "/b_00_02_part3"),
                                    pattern = ".tif",
                                    full.names = TRUE))

batch_2000 <- rast(listFILES[1:3])

dimsReport(path = paste0(getwd(), "/b_00_02_part3"), message = NULL)

waysToSplit(h=699, v=150, batch_2000)

split_replace_terra(raster=batch_2000, h=23, v=48, 
                    outputPath = "/home/series_tiempo/Escritorio/proyectoShantall/b_00_02_part3/gapfill/splits/2000",
                    name="2000")



