# Readin Routine for Image Patches
# 
# Author: Anton Becker (github.com/ASBecker)
# License: MIT
#
# Returns functions for Image data readin/list generation
# Currently works for two classes of images
# Point "base_dir" to the folder where your images are.
# If you run experiments with a fixed split, put the images in
# folders called "train" and "validation" in folders with the
# class names. If you want this script to randomly split the
# data just put them in class-folders in the "base_dir".
# You can specify a seed for the split for reproducibility.

library(magrittr)
library(png)
library(stringr)

base_dir <- 'Data/patches/png/'
class1 <- 'controls'
class2 <- 'treated'

#co_ids <- list.files('Data/full_pics/controls/') %>% sapply(., str_replace, '.tif', '')
#tr_ids <- list.files('Data/full_pics/ablated/') %>% sapply(., str_replace, '.tif', '')

img_fixed_split <- function(){
  # Analogous behaviour to the keras::flow_images_from_directory() function
  co_patches_train <- list.files(paste0(base_dir, 'train/', class1), 
                                        '.png', full.names=TRUE)
  tr_patches_train <- list.files(paste0(base_dir, 'train/', class2), 
                                        '.png', full.names=TRUE)
  co_patches_valid <- list.files(paste0(base_dir, 'validation/', class1),
                                        '.png', full.names=TRUE)
  tr_patches_valid <- list.files(paste0(base_dir, 'validation/', class2),
                                        '.png', full.names=TRUE)
  
  d.img <- list(
    train = list(
      x = sapply(c(co_patches_train, tr_patches_train), readPNG, simplify = 'array') %>% 
        aperm(c(4,1,2,3)) %>% .[,,,c(1:3)],
      y = c(rep(1, length(co_patches_train)), rep(0, length(tr_patches_train)))
    ),
    test = list(
      x = sapply(c(co_patches_valid, tr_patches_valid), readPNG, simplify = 'array') %>% 
        aperm(c(4,1,2,3)) %>% .[,,,c(1:3)],
      y = c(rep(1, length(co_patches_valid)), rep(0, length(tr_patches_valid)))
    )
  )
  rownames(d.img$train$x) <- NULL
  rownames(d.img$test$x) <- NULL

  return(d.img)
}


img_random_split <- function(train_split=0.8, seed, shuffle=TRUE){
  
  if (train_split >= 1 | train_split <= 0) {
    stop('Invalid split, please select a value between 0 and 1')
    }

  try(if (exists('seed')) {
    old <- .Random.seed
    on.exit( { .Random.seed <<- old } )
    set.seed(seed)
  }, silent=TRUE)
  
  co_patches <- list.files(paste0(base_dir, class1, '.png'), full.names=TRUE)
  tr_patches <- list.files(paste0(base_dir, class2, '.png'), full.names=TRUE)

  x_full <- sapply(c(co_patches, tr_patches), readPNG, simplify = 'array') %>% 
               aperm(c(4,1,2,3)) %>% .[,,,c(1:3)]
  y_full <- c(rep(1, length(co_patches)), rep(0, length(tr_patches)))
  
  #x_full %<>% gRbase::ar_normalize(type='all') # Normalization over all patches
  
  co_range <- c(1:length(co_patches))
  tr_range <- c((length(co_patches)+1):(length(tr_patches)+length(co_patches)))
  
  n_co_train <- (length(co_patches)*train_split) %>% round()
  n_tr_train <- (length(tr_patches)*train_split) %>% round()
  
  co_train_ind <- sample(co_range, n_co_train)
  co_test_ind <- co_range[which(!co_range %in% co_train_ind)]
  tr_train_ind <- sample(tr_range, n_tr_train)
  tr_test_ind <- tr_range[which(!tr_range %in% tr_train_ind)]
  
  train_ind <- c(co_train_ind, tr_train_ind)
  test_ind <- c(co_test_ind, tr_test_ind)

  if (shuffle=TRUE) {
    train_ind %<>% .[sample(c(1:length(.)))]
    test_ind %<>% .[sample(c(1:length(.)))]
  }

  d.img <- list(
    train = list(
      x = x_full[train_ind,,,],
      y = y_full[train_ind]
    ),
    test = list(
      x = x_full[test_ind,,,],
      y = y_full[test_ind]
    )
  )
  rownames(d.img$train$x) <- NULL
  rownames(d.img$test$x) <- NULL
  return(d.img)
}