# Load PHYLACINE 1.2.1 maps
# 22/07-2021 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)
library(raster)
library(Matrix)
library(rgdal)
library(doSNOW)
library(tictoc)

# Load the filtered dataset
df <- read_csv("builds/data.csv")



# Find files ---------------------------------------------------------------

# Set paths to PHYLACINE maps
current.maps.path <- "../PHYLACINE_1.2/Data/Ranges/Current/"
present.natural.maps.path <- "../PHYLACINE_1.2/Data/Ranges/Present_natural/"

# Check that all current map files are present and filter to our dataset
files <- dir(current.maps.path)
species <- str_replace(files, "\\.tif", "")
current.maps.df <- tibble(bin = species, map = paste0(current.maps.path, files))
stopifnot(df$Binomial.1.2 %in% current.maps.df$bin) # Sensibility check
current.maps.df <- current.maps.df %>%
  filter(bin %in% df$Binomial.1.2) # Filter to only terrestrial species
stopifnot(all.equal(current.maps.df$bin, df$Binomial.1.2)) # Alignment check

# Check that all present natural map files are present and filter to our dataset
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
writeRaster(base.map, "builds/base_map.tif", overwrite = TRUE)




# Load ranges -------------------------------------------------------------

# Setup parallel cluster
cluster.size <- 6
cl <- makeCluster(cluster.size)
registerDoSNOW(cl)

n <- nrow(current.maps.df)
pb <- txtProgressBar(max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Load current maps
timestamp()
tic()
current.maps <- foreach(i = 1:n,                         
                        .packages = c('rgdal', "Matrix"), 
                        .combine = rbind,
                        .inorder = TRUE,
                        .options.snow = opts) %dopar% {
                          # Load map values
                          map.values <- readGDAL(current.maps.df$map[i], silent = TRUE)@data$band1
                          
                          # Return as sparse matrix
                          t(as(map.values, "sparseMatrix"))
                        }

toc()
rownames(current.maps) <- current.maps.df$bin
write_rds(current.maps, file = "builds/current.maps.filtered.rds")


# Load present natural maps
timestamp()
tic()
present.natural.maps <- foreach(i = 1:n,                         
                                .packages = c('rgdal', "Matrix"), 
                                .combine = rbind,
                                .inorder = TRUE,
                                .options.snow = opts) %dopar% {
                                  # Load map values
                                  map.values <- readGDAL(present.natural.maps.df$map[i], silent = TRUE)@data$band1
                                  
                                  # Return as sparse matrix
                                  t(as(map.values, "sparseMatrix"))
                                }

toc()
rownames(present.natural.maps) <- present.natural.maps.df$bin
write_rds(present.natural.maps, file = "builds/present.natural.maps.filtered.rds")

stopCluster(cl)
gc()




# Check files -------------------------------------------------------------

# current.maps <- readRDS("builds/current.maps.filtered.rds")
# present.natural.maps <- readRDS("builds/present.natural.maps.filtered.rds")

library(tools)
# Data from Phylacine v. 1.2.1
sum(current.maps)
# 728143
md5sum("builds/current.maps.filtered.rds")
# "3317bd315dce4dce5b738cf4d5eed1db" 
stopifnot(md5sum("builds/current.maps.filtered.rds") == "3317bd315dce4dce5b738cf4d5eed1db")

sum(present.natural.maps)
# 946513
md5sum("builds/present.natural.maps.filtered.rds")
# "a20a4bf001184985b958065c50a97f2e"
stopifnot(md5sum("builds/present.natural.maps.filtered.rds") == "a20a4bf001184985b958065c50a97f2e")


