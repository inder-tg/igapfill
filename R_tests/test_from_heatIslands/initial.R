
# --- June 25, 2024

# --- ORIGINALLY taken from heatIslands R project
# --- CODE below produces a "sieve of missing data" using a heatmaply object
# --- ORIGINAL files were stored in subdirectories; each subdirectory was linked 
# --- to a year in the studied period. 

library(terra)
library(gtools)
library(heatmaply)
library(igapfill)
library(raster)
library(sta)
library(doParallel)

dataDIRS <- list.dirs(path=paste0(getwd(), "/data/cell_165_original"))

dataDIRS <- dataDIRS[-c(1)]

# ---

percentMat <- matrix(nrow=length(dataDIRS),ncol=12)
totalPixels <- 36 * 36

for(i in seq_len(nrow(percentMat))){
  TIFs <- mixedsort(list.files(path = dataDIRS[i], full.names = TRUE))
  for(j in seq_len(ncol(percentMat))){
    r <- rast(TIFs[j])
    percentMat[i,j] <- as.numeric(global(r, fun="isNA")) / totalPixels
    # plot(r, main=names(r))
  }
}

row.names(percentMat) <- 2000:2022
colnames(percentMat) <- month.name

# ---

heatmaply(percentMat,
          limits=c(0,1),
          colors = cool_warm,
          dendrogram="none",
          xlab="", ylab="",
          main="original",
          scale = "none",
          draw_cellnote = TRUE,
          margins=c(60,100,40,20),
          grid_color="white",
          grid_width=0.00001,
          titleX = FALSE,
          hide_colorbar = TRUE,
          labCol = colnames(percentMat),
          labRow = rownames(percentMat),
          heatmap_layers = theme(axis.line=element_blank())
)

# ---

