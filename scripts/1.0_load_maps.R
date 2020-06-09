# Load packages
library(tidyverse)
library(raster)
library(doSNOW)
library(tictoc)

cluster.size <- 6

# Load maps:
current.maps.path <- "../PHYLACINE_1.2/Data/Ranges/Current/"
present.natural.maps.path <- "../PHYLACINE_1.2/Data/Ranges/Present_natural/"

files <- dir(current.maps.path)
species <- str_replace(files, "\\.tif", "")
current.maps.df <- tibble(bin = species, map = paste0(current.maps.path, files))

files <- dir(present.natural.maps.path)
species <- str_replace(files, "\\.tif", "")
present.natural.maps.df <- tibble(bin = species, map = paste0(present.natural.maps.path, files))

## Load tiff ranges
cl <- makeCluster(cluster.size)
registerDoSNOW(cl)

n <- nrow(current.maps.df)
pb <- txtProgressBar(max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

timestamp()
tic()
current.maps <- foreach(i = 1:n,                         
                        .packages = c('raster'), 
                        .combine = rbind,
                        .inorder = TRUE,
                        .options.snow = opts) %dopar% {
                          getValues(raster(current.maps.df$map[i]))
                        }

toc()
rownames(current.maps) <- current.maps.df$bin
save(current.maps, file = "builds/current.maps.RData")

n <- nrow(present.natural.maps.df)
pb <- txtProgressBar(max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

timestamp()
tic()
present.natural.maps <- foreach(i = 1:n,                         
                                .packages = c('raster'), 
                                .combine = rbind,
                                .inorder = TRUE,
                                .options.snow = opts) %dopar% {
                                  getValues(raster(present.natural.maps.df$map[i]))
                                }

toc()
rownames(present.natural.maps) <- present.natural.maps.df$bin
save(present.natural.maps, file = "builds/present.natural.maps.RData")

stopCluster(cl)

# load("builds/current.maps.RData")
# load("builds/present.natural.maps.RData")
