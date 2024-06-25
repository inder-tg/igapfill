
library(igapfill)
library(gtools)

igapfill()

# --- mosaicking

library(igapfill)
library(foreach)
library(doParallel)
library(gtools)
library(itertools)

source(paste0( getwd(), "/Rscripts/auxFUN.R" ))

# parallel_mosaicking <- function(inputDirImages,
#                                 inputDirRData,
#                                 inputDirMaster,
#                                 outputDir,
#                                 progressReportDir,
#                                 numCores=23){
#   
#   initTEXT <- paste0("Loading arguments: ", textColor("original TIF files,", color="yellow"),
#                      textColor(" RData files,", color="red"), textColor(" master files, ", color = "blue"),
#                      textColor(" etc.", color = "green"))
#   
#   message(initTEXT)
# 
#   imagesToProcess <- mixedsort(list.files(path=inputDirImages,
#                                           pattern = ".tif",
#                                           full.names = TRUE))
#   
#   RDataToProcess <- mixedsort(list.files(path = inputDirRData,
#                                          pattern = ".RData",
#                                          full.names = TRUE))
#   
#   masterToProcess <- mixedsort(list.files(path = inputDirMaster,
#                                           pattern = ".tif",
#                                           full.names = TRUE))
#   
#   totalListFILES <- sqrt(length(imagesToProcess))
#   
#   for(i in 2:totalListFILES){
#     for(j in 1:totalListFILES){
#       listRaster <- getMosaicList(i=i,j=j,
#                                   imagesToProcess = imagesToProcess,
#                                   RDataToProcess = RDataToProcess,
#                                   masterToProcess = masterToProcess,
#                                   progressReportDir = progressReportDir,
#                                   numCores = numCores)
#       
#       doMosaicking(rlist=listRaster, numCores=numCores,
#                    originalRaster=imagesToProcess[totalListFILES*(i-1)+j], 
#                    outputDir=outputDir,
#                    progressReportDir=progressReportDir)
#     }
#   }
#   
#   outputMessage <- paste0( "See ", textColor(text=outputDir, color = "red") )
#   message(outputMessage)
# }


inputDirImages="/home/series_tiempo/Escritorio/proyectoShantall/b_00_02_part3"
inputDirRData="/home/series_tiempo/Escritorio/proyectoShantall/b_00_02_part3/gapfill/output"
inputDirMaster="/home/series_tiempo/Escritorio/proyectoShantall/b_00_02_part3/master"
outputDir="/home/series_tiempo/Escritorio/proyectoShantall/b_00_02_part3/gapfill/filled"
progressReportDir="/home/series_tiempo/Escritorio/proyectoShantall/b_00_02_part3/gapfill/progressReports"
numCores=15

parallel_mosaicking(inputDirImages = inputDirImages,
                    inputDirRData = inputDirRData,
                    inputDirMaster = inputDirMaster,
                    outputDir = outputDir,
                    progressReportDir = progressReportDir,
                    numCores = 15)



