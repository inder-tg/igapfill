
getBlock2 <- function(row, col, sieve){
  
  TOP <- (row-1):row
  LEFT <- (col-1):col
  RIGHT <- col:(col+1)
  BOTTOM <- row:(row+1)
  
  blocksGrid <- vector(mode = "list", length = 4)
  
  blocksGrid[[1]] <- sieve[ TOP, LEFT ]
  
  blocksGrid[[2]] <- sieve[ TOP, RIGHT ]
  
  blocksGrid[[3]] <- sieve[ BOTTOM, LEFT ]
  
  blocksGrid[[4]] <- sieve[ BOTTOM, RIGHT ]
  
  blockProducts <- lapply(1:length(blocksGrid), 
                          function(s) as.numeric(cumprod( blocksGrid[[s]] ) ))
  
  BLOCK <- which.min(lapply(1:length(blocksGrid), 
                            function(s) blockProducts[[s]][length(blockProducts[[s]])] ))
  
  if(BLOCK == 1){
    ROWS <- TOP
    COLS <- LEFT
  }
  
  if(BLOCK == 2){
    ROWS <- TOP
    COLS <- RIGHT
  }
  
  if(BLOCK == 3){
    ROWS <- BOTTOM
    COLS <- LEFT
  }
  
  if(BLOCK == 4){
    ROWS <- BOTTOM
    COLS <- RIGHT
  }
  
list(rows = ROWS, cols = COLS,      
     block=blocksGrid[[BLOCK]],
     blockMissingness = blockProducts[[BLOCK]][length(blockProducts)])
}

getBlock3 <- function(row, col, sieve){
  TOP <- (row-2):row
  LEFT <- (col-2):col
  RIGHT <- col:(col+2)
  BOTTOM <- row:(row+2)
  SIDErow <- (row-1):(row+1)
  SIDEcol <- (col-1):(col+1)
  
  blocksGrid <- vector(mode = "list", length = 9)
  
  blocksGrid[[1]] <- sieve[ TOP, LEFT ]
  
  blocksGrid[[2]] <- sieve[ TOP, RIGHT ]
  
  blocksGrid[[3]] <- sieve[ BOTTOM, LEFT ]
  
  blocksGrid[[4]] <- sieve[ BOTTOM, RIGHT ]
  
  blocksGrid[[5]] <- sieve[ SIDErow, LEFT ]
  
  blocksGrid[[6]] <- sieve[ SIDErow, RIGHT ]
  
  blocksGrid[[7]] <- sieve[ TOP, SIDEcol ]
  
  blocksGrid[[8]] <- sieve[ BOTTOM, SIDEcol ]
  
  blocksGrid[[9]] <- sieve[ SIDErow, SIDEcol ]
  
  blockProducts <- lapply(1:length(blocksGrid), 
                          function(s) as.numeric(cumprod( blocksGrid[[s]] ) ))
  
  BLOCK <- which.min(lapply(1:length(blocksGrid), 
                            function(s) blockProducts[[s]][length(blockProducts[[s]])] ))
  
  if(BLOCK == 1){
    ROWS <- TOP
    COLS <- LEFT
  }
  
  if(BLOCK == 2){
    ROWS <- TOP
    COLS <- RIGHT
  }
  
  if(BLOCK == 3){
    ROWS <- BOTTOM
    COLS <- LEFT
  }
  
  if(BLOCK == 4){
    ROWS <- BOTTOM
    COLS <- RIGHT
  }
  
  if(BLOCK == 5){
    ROWS <- SIDErow
    COLS <- LEFT
  }
  
  if(BLOCK == 6){
    ROWS <- SIDErow
    COLS <- RIGHT
  }
  
  if(BLOCK == 7){
    ROWS <- TOP
    COLS <- SIDEcol
  }
  
  if(BLOCK == 8){
    ROWS <- BOTTOM
    COLS <- SIDEcol
  }
  
  if(BLOCK==9){
    ROWS <- SIDErow
    COLS <- SIDEcol
  }
  
  list(rows = ROWS, cols = COLS,      
       block=blocksGrid[[BLOCK]],
       blockMissingness = blockProducts[[BLOCK]][length(blockProducts)])
}

getMinBlock <- function(sieve, 
                        row, col,
                        blockSize=2){
  
  if( !inherits(sieve, "matrix" ) ){
    stop("sieve must be a squared matrix")
  }
  
  if( blockSize >= 4 ){
    stop("method not implemented for this argument value")
  }
  
  if(blockSize==2){
    out <- getBlock2(row=row, col=col, sieve=sieve)
  }
  
  if(blockSize==3){
    out <- getBlock3(row=row, col=col, sieve=sieve)
  }
  
list(rows = out$rows,
     cols = out$cols,
     block = out$block, #blocksGrid[[BLOCK]],
     blockMissingness = out$blockMissingness) # blockProducts[[BLOCK]][length(blockProducts)])
}

findMinCell <- function(sieve,k){
  ord_sieve <- order(sieve, decreasing = FALSE)[1:k]

  if( length( which(sieve[ord_sieve] > 0) ) == 0 ){
    stop("Try increasing the value of k")
  } else {
    sieve_ind <- which(sieve == sieve[ord_sieve[which(sieve[ord_sieve] > 0)[1]]], 
                        arr.ind = TRUE) 
  }
  
list(position=ord_sieve, indices=sieve_ind)
}

findMinCell(sieve = original_sieve, k=3)
findMinCell(sieve = test_sieve, k=5)

# --- Added on July 3, 2024

getMASTER <- function(path, outputPath){
  
  if(missing(path) | missing(outputPath)){
    stop("'path' and 'outputPath' must be provided")
  }
  
  pathFILES <- list.files(path = path,
                          pattern = ".tif",
                          full.names = TRUE)
  STACK <- stack(pathFILES)
  
  MASTER <- subset(STACK,1)
  MASTER[is.na(MASTER)] <- 1L
  
  writeRaster(x=MASTER,
              filename = paste0(outputPath, "/master"),
              format="GTiff", datatype="INT1U",
              overwrite=TRUE)
}








