# Load packages
library(tidyverse)
library(raster)
library(doSNOW)
library(tictoc)

df <- read_csv("builds/data.csv")

# Load maps:
current.maps.path <- "../PHYLACINE_1.2/Data/Ranges/Current/"
present.natural.maps.path <- "../PHYLACINE_1.2/Data/Ranges/Present_natural/"

files <- dir(current.maps.path)
species <- str_replace(files, "\\.tif", "")
current.maps.df <- tibble(bin = species, map = paste0(current.maps.path, files))
stopifnot(df$Binomial.1.2 %in% current.maps.df$bin) # Sensibility check
current.maps.df <- current.maps.df %>%
  filter(bin %in% df$Binomial.1.2) # Filter to only terrestrial species
stopifnot(all.equal(current.maps.df$bin, df$Binomial.1.2)) # Alignment check

files <- dir(present.natural.maps.path)
species <- str_replace(files, "\\.tif", "")
present.natural.maps.df <- tibble(bin = species, map = paste0(present.natural.maps.path, files))
stopifnot(df$Binomial.1.2 %in% present.natural.maps.df$bin) # Sensibility check
present.natural.maps.df <- present.natural.maps.df %>%
  filter(bin %in% df$Binomial.1.2) # Filter to only terrestrial species
stopifnot(all.equal(present.natural.maps.df$bin, df$Binomial.1.2)) # Alignment check

# Create base map
base.map <- raster(current.maps.df$map[1])
base.map[] <- NA
writeRaster(base.map, "builds/base_map.tif")

cluster.size <- 6

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
save(current.maps, file = "builds/current.maps.filtered.RData")

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
save(present.natural.maps, file = "builds/present.natural.maps.filtered.RData")

stopCluster(cl)

# load("builds/current.maps.RData")
# load("builds/present.natural.maps.RData")

library(tools)
# Data from Phylacine v. 1.2.1
sum(current.maps)
# 745516
md5sum("builds/current.maps.filtered.RData")
# "a0af2cce75e3785678389985d45b9b3f" 
sum(present.natural.maps)
# 956180
md5sum("builds/present.natural.maps.filtered.RData")
# "ea21960aa3dabbfd99a1ac142e3b65af"
