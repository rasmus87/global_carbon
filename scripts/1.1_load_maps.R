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
                        .packages = c('rgdal'), 
                        .combine = rbind,
                        .inorder = TRUE,
                        .options.snow = opts) %dopar% {
                          # getValues(raster(current.maps.df$map[i]))
                          readGDAL(current.maps.df$map[i], silent = TRUE)@data$band1
                        }

toc()
rownames(current.maps) <- current.maps.df$bin
mode(current.maps) <- "integer"
saveRDS(current.maps, file = "builds/current.maps.filtered.rds")

n <- nrow(present.natural.maps.df)
pb <- txtProgressBar(max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

timestamp()
tic()
present.natural.maps <- foreach(i = 1:n,                         
                                .packages = c('rgdal'), 
                                .combine = rbind,
                                .inorder = TRUE,
                                .options.snow = opts) %dopar% {
                                  # getValues(raster(present.natural.maps.df$map[i]))
                                  readGDAL(present.natural.maps.df$map[i], silent = TRUE)@data$band1
                                }

toc()
rownames(present.natural.maps) <- present.natural.maps.df$bin
mode(present.natural.maps) <- "integer"
saveRDS(present.natural.maps, file = "builds/present.natural.maps.filtered.rds")

stopCluster(cl)

# current.maps <- readRDS("builds/current.maps.rds")
# present.natural.maps <- readRDS("builds/present.natural.maps.rds")

library(tools)
# Data from Phylacine v. 1.2.1
sum(current.maps)
# 745516
md5sum("builds/current.maps.filtered.rds")
# "8e4042ce20ab692b6b58f3eb362ec613" 
stopifnot(md5sum("builds/current.maps.filtered.rds") == "8e4042ce20ab692b6b58f3eb362ec613")

sum(present.natural.maps)
# 956180
md5sum("builds/present.natural.maps.filtered.rds")
# "b1c03ea7178e54f19d761a8065fb98a1"
stopifnot(md5sum("builds/present.natural.maps.filtered.rds") == "b1c03ea7178e54f19d761a8065fb98a1")


