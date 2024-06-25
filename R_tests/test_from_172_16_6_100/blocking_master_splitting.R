
# --- June 25, 2024
# --- TAKEN from heatIsland project written @172.16.6.100 machine
# --- BELOW is a trial and error procedure to "automatically" select the best
# --- BLOCK candidate for the application of igapfill() in the next iteration. I admit,
# --- it is not the best code ever, but it helped to move forward in the heatIslands
# --- project

library(gtools)

# --- block1: 2002-2003, Feb-March

test_block1 <- which(original_sieve==min(original_sieve), arr.ind = TRUE)

init_year <- test_block1[[1]]
init_image <- test_block1[[2]]+1
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block1"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block1"))
}

# # --- block1: splitting master file
# 
# gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"
# 
# dir.create(path = paste0(gapfillDIR, "/block1/gapfill/master"))
# 
# listFILES <- mixedsort(list.files(path = paste0(rootDIR, "/data_gapfill/block1"),
#                         pattern = ".tif",
#                         full.names = TRUE))
# 
# test <- rast(listFILES[1])
# 
# as.numeric(global(test, fun="isNA")) / (nrow(test) * ncol(test))
# 
# test[is.na(test)] <- 1L
# 
# waysToSplit(h=45,v=40,raster=test)
# 
# 1560/40
# 2250/45
# 
# ?split_master
# split_master(x=test, 
#              outputPath=paste0(gapfillDIR, "/block1/gapfill/master"), 
#              h=50, v=39)

# --- block 2: 2016-2017, Jan-Feb

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA

test_block2 <- which(block1_sieve==min(block1_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block2

# block1_sieve_2 <- block1_sieve
block1_sieve[3,1] <- NA
test_block2 <- which(block1_sieve==min(block1_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block2

# ---

init_year <- 17
init_image <- 1
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block2"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block2"))
}

# --- block3: 2020-2021, Sep-Oct

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

test_block3 <- which(block2_sieve==min(block2_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block3

# --- 

init_year <- 21
init_image <- 9
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block3"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block3"))
}

# --- block 4: 2021-2022, March-April

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

test_block4 <- which(block3_sieve==min(block3_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block4

# ---

init_year <- 22
init_image <- 3
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block4"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block4"))
}

# ---

# --- block 5: 2021-2022, June-July

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

test_block5 <- which(block4_sieve==min(block4_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block5

# ---

init_year <- 22
init_image <- 6
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block5"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block5"))
}

# --- block6: 2001-2002; Nov-Dec

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

test_block6 <- which(block5_sieve==min(block5_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block6

init_year <- 2
init_image <- 11
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block6"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block6"))
}

# --- block7: 2014-2015; Aug-Sep

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

test_block7 <- which(block6_sieve==min(block6_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block7

init_year <- 15
init_image <- 8
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block7"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block7"))
}

# ---

# --- block8: 

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA
  
test_block8 <- which(block7_sieve==min(block7_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block8

init_year <- 14
init_image <- 3
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block8"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block8"))
}
  

# --- block9: 2011-2012, Aug-Sep

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

test_block9 <- which(block8_sieve==min(block8_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block9

init_year <- 12
init_image <- 8
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block9"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block9"))
}

# ---

# --- block10: 2013-2014, Oct-Nov

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

test_block10 <- which(block9_sieve==min(block9_sieve, na.rm = TRUE), 
                     arr.ind = TRUE)
test_block10

init_year <- 14
init_image <- 10
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block10"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block10"))
}

# --- block11: 2021-2022; Enero-Feb

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

test_block11 <- which(block10_sieve==min(block10_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block11

init_year <- 22
init_image <- 1
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block11"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block11"))
}

# --- block12: 2016-2017, Oct-Nov

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

test_block12 <- which(block11_sieve==min(block11_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block12

init_year <- 17
init_image <- 10
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block12"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block12"))
}


# --- block13: 2018-2019; Marzo-Abril

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

test_block13 <- which(block12_sieve==min(block12_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block13

init_year <- 19
init_image <- 4
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block13"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block13"))
}


# --- block14: 2011-2012; Abril-Mayo

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

test_block14 <- which(block13_sieve==min(block13_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block14

init_year <- 12
init_image <- 4
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block14"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block14"))
}


# --- block15: 2000-2001; Agosto-Sep

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

test_block15 <- which(block14_sieve==min(block14_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block15

init_year <- 1
init_image <- 8
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block15"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block15"))
}


# --- block16: 2003-2004; Nov-Dec

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

block15_sieve <- block14_sieve
block15_sieve[1:2, 8:9] <- NA

block15_sieve[17, 5] <- NA

test_block16 <- which(block15_sieve==min(block15_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block16

init_year <- 4
init_image <- 11
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block16"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block16"))
}

# ---


# --- block17: 2015-2016; june-July

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

block15_sieve <- block14_sieve
block15_sieve[1:2, 8:9] <- NA

block15_sieve[17, 5] <- NA

block16_sieve <- block15_sieve
block16_sieve[4:5,11:12] <- NA

test_block17 <- which(block16_sieve==min(block16_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block17

init_year <- 16
init_image <- 6
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block17"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block17"))
}



# --- block18: 2016-2017; abril-mayo

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

block15_sieve <- block14_sieve
block15_sieve[1:2, 8:9] <- NA

block15_sieve[17, 5] <- NA

block16_sieve <- block15_sieve
block16_sieve[4:5,11:12] <- NA

block17_sieve <- block16_sieve
block17_sieve[16:17,6:7] <- NA

# block18_sieve <- block17_sieve

test_block18 <- which(block17_sieve==min(block17_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block18

init_year <- 17
init_image <- 5
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block18"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block18"))
}

# --- block19:  2016-2017, Mar-Abril

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

block15_sieve <- block14_sieve
block15_sieve[1:2, 8:9] <- NA

block15_sieve[17, 5] <- NA

block16_sieve <- block15_sieve
block16_sieve[4:5,11:12] <- NA

block17_sieve <- block16_sieve
block17_sieve[16:17,6:7] <- NA

block18_sieve <- block17_sieve
block18_sieve[17:18,5:6] <- NA

test_block19 <- which(block18_sieve==min(block18_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block19

init_year <- 17
init_image <- 3
blockDIRS <- dataDIRS[init_year:(init_year+1)]
whatImages <- init_image:(init_image+1)

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block19"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block19"))
}


# --- block20:  2010-2011, Oct-Nov

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

block15_sieve <- block14_sieve
block15_sieve[1:2, 8:9] <- NA

block15_sieve[17, 5] <- NA

block16_sieve <- block15_sieve
block16_sieve[4:5,11:12] <- NA

block17_sieve <- block16_sieve
block17_sieve[16:17,6:7] <- NA

block18_sieve <- block17_sieve
block18_sieve[17:18,5:6] <- NA

block19_sieve <- block18_sieve
block19_sieve[17:18,3:4] <- NA

test_block20 <- which(block19_sieve==min(block19_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block20

init_year <- 11
init_image <- 10
(blockDIRS <- dataDIRS[init_year:(init_year+1)])
(whatImages <- init_image:(init_image+1))

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block20"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block20"))
}


# --- block21:  2003-2004, Abril-Mayo

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

block15_sieve <- block14_sieve
block15_sieve[1:2, 8:9] <- NA

block15_sieve[17, 5] <- NA

block16_sieve <- block15_sieve
block16_sieve[4:5,11:12] <- NA

block17_sieve <- block16_sieve
block17_sieve[16:17,6:7] <- NA

block18_sieve <- block17_sieve
block18_sieve[17:18,5:6] <- NA

block19_sieve <- block18_sieve
block19_sieve[17:18,3:4] <- NA

block20_sieve <- block19_sieve
block20_sieve[11:12,10:11] <- NA

test_block21 <- which(block20_sieve==min(block20_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block21

init_year <- 4
init_image <- 4
(blockDIRS <- dataDIRS[init_year:(init_year+1)])
(whatImages <- init_image:(init_image+1))

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block21"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block21"))
}



# --- block22:  2018-2019, july-august

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

block15_sieve <- block14_sieve
block15_sieve[1:2, 8:9] <- NA

block15_sieve[17, 5] <- NA

block16_sieve <- block15_sieve
block16_sieve[4:5,11:12] <- NA

block17_sieve <- block16_sieve
block17_sieve[16:17,6:7] <- NA

block18_sieve <- block17_sieve
block18_sieve[17:18,5:6] <- NA

block19_sieve <- block18_sieve
block19_sieve[17:18,3:4] <- NA

block20_sieve <- block19_sieve
block20_sieve[11:12,10:11] <- NA

block21_sieve <- block20_sieve
block21_sieve[4:5, 4:5] <- NA

test_block22 <- which(block21_sieve==min(block21_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block22

init_year <- 19
init_image <- 7
(blockDIRS <- dataDIRS[init_year:(init_year+1)])
(whatImages <- init_image:(init_image+1))

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block22"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block22"))
}


# --- block23:  2018-2019, nov-dec

block1_sieve <- gapfill_sieve
block1_sieve[3:4,2:3] <- NA
block1_sieve[3,1] <- NA

block2_sieve <- block1_sieve
block2_sieve[17:18,1:2] <- NA

block3_sieve <- block2_sieve
block3_sieve[21:22, 9:10] <- NA

block4_sieve <- block3_sieve
block4_sieve[22:23, 3:4] <- NA

block5_sieve <- block4_sieve
block5_sieve[22:23,6:7] <- NA

block5_sieve[16,7] <- NA

block6_sieve <- block5_sieve
block6_sieve[2:3,11:12] <- NA

block7_sieve <- block6_sieve
block7_sieve[15:16, 8:9] <- NA

block8_sieve <- block7_sieve
block8_sieve[14:15,3:4] <- NA

block9_sieve <- block8_sieve
block9_sieve[12:13,8:9] <- NA

block10_sieve <- block9_sieve
block10_sieve[14:15,10:11] <- NA

block10_sieve[17,3] <- NA

block11_sieve <- block10_sieve
block11_sieve[22:23,1:2] <- NA

block12_sieve <- block11_sieve
block12_sieve[17:18,10:11] <- NA

block13_sieve <- block12_sieve
block13_sieve[19:20,4:5] <- NA

block14_sieve <- block13_sieve
block14_sieve[12:13, 4:5] <- NA

block15_sieve <- block14_sieve
block15_sieve[1:2, 8:9] <- NA

block15_sieve[17, 5] <- NA

block16_sieve <- block15_sieve
block16_sieve[4:5,11:12] <- NA

block17_sieve <- block16_sieve
block17_sieve[16:17,6:7] <- NA

block18_sieve <- block17_sieve
block18_sieve[17:18,5:6] <- NA

block19_sieve <- block18_sieve
block19_sieve[17:18,3:4] <- NA

block20_sieve <- block19_sieve
block20_sieve[11:12,10:11] <- NA

block21_sieve <- block20_sieve
block21_sieve[4:5, 4:5] <- NA

block22_sieve <- block21_sieve
block22_sieve[19:20,7:8] <- NA


test_block23 <- which(block22_sieve==min(block22_sieve, na.rm = TRUE), 
                      arr.ind = TRUE)
test_block23

init_year <- 19
init_image <- 11
(blockDIRS <- dataDIRS[init_year:(init_year+1)])
(whatImages <- init_image:(init_image+1))

rootDIR <- "/home/itecuapetla/Desktop/heatIsland"
gapfillDIR <- "/home/itecuapetla/Desktop/heatIsland/data_gapfill"

dir.create(path = paste0(gapfillDIR, "/block23"))

for(i in blockDIRS){
  listFILES <- mixedsort(list.files(path=i,
                                    pattern = ".tif",
                                    full.names = TRUE) )
  file.copy(from=listFILES[whatImages], to=paste0(gapfillDIR, "/block23"))
}




