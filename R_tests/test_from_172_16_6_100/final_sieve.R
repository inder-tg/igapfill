
library(gtools)
library(heatmaply)
library(terra)

# ---

testDIR <- "/home/itecuapetla/Desktop/heatIsland/data_copy_gapfill"
dataDIRS <- list.dirs(path=testDIR)
dataDIRS <- dataDIRS[-1]

gapfill_sieve <- missVal_sieve(DIR=dataDIRS,
                            amountFiles = 12,
                            totalPixels = 1560 * 2250,
                            startYear = 2000,
                            endYear = 2022)
# width=1046
# height=474

heatmaply(gapfill_sieve,
          limits=c(0,1),
          colors = cool_warm,
          dendrogram="none",
          xlab="", ylab="",
          main="% Missing Data (after Block22)",
          scale = "none",
          draw_cellnote = TRUE,
          margins=c(60,100,40,20),
          grid_color="white",
          grid_width=0.00001,
          titleX = FALSE,
          hide_colorbar = TRUE,
          labCol = colnames(gapfill_sieve),
          labRow = rownames(gapfill_sieve),
          heatmap_layers = theme(axis.line=element_blank())
)

