# Generates square tiles from large images
# for further usage with Keras and/or Tensorflow
# 
# Author: Anton Becker (github.com/ASBecker)
# License: MIT

library(tiff)
library(png)
library(magrittr)
library(stringr)

# Set User Defined Parameters -----------------------------------------------

tsz <- 50 # px

in_path <- 'Data/full_pics/controls/'
out_path <- 'Data/patches/png/controls/' # needs trailing slash!

# Main Section --------------------------------------------------------------

fileList <- list.files(in_path)

# Start of function:
f.imgsplit <- function(file, in.path, out.path) {

  my.img <- readTIFF(paste0(in.path,file))
  n.tiles <- dim(my.img)[1:2] %/% tsz # may discard right/lower border
  
  d.tiles <- data.frame(matrix(NA, ncol=2, nrow=n.tiles[1]*n.tiles[2]))
  colnames(d.tiles) <- c('tile','Data')
  d.tiles$Data <- rep(list(array(0, dim=c(tsz, tsz, dim(my.img)[3]))), nrow(d.tiles))
  d.tiles$tile <- c(1:nrow(d.tiles))
  out.base <- str_split(file, coll('.tif'))[[1]][1]
  
  el <- 0
  
  # Actual Tile Splitting:  
  for (i in seq(tsz, dim(my.img)[2], by=tsz)) { # Rows
    for (j in seq(tsz, dim(my.img)[1], by=tsz)) { # Columns
      el <- el+1
      d.tiles$Data[[el]] <- my.img[(j-(tsz-1)):j, (i-(tsz-1)):i, 1:dim(my.img)[3]]
    }
  }
  
  # Write tiles to individual files:  
  for (n in c(1:el)) {
    out.name <- paste0(out.path, out.base, '_',as.character(n),'.png')
    if (all(d.tiles$Data[[n]]!=0)) {
      writePNG(d.tiles$Data[[n]], out.name)
    }
  }
}
# End of function

# Do for all files in in_folder:
sapply(fileList, f.imgsplit, in_path, out_path)
