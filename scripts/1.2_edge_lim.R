# Load the filtered maps, and add a reduction factor to the range outline
# 22/07-2021 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)
library(raster)
library(Matrix)

# Load base map
base.map <- raster("builds/base_map.tif")



# Build a edge reduction function -----------------------------------------

# Make a matrix of neighbour cells:
rows <- nrow(base.map)
cols <- ncol(base.map)
N = cols * rows
neighbours <- matrix(nrow = N, ncol = 4)
for(i in 1:N) {
  # left
  if(i %% 360 == 1) {
    # left column
    left = i - 1 + cols
  } else {
    left = i - 1
  }
  
  # right
  if(i %% 360 == 0) {
    # right column
    right = i + 1 - cols
  } else {
    right = i + 1
  }
  
  # above
  if(i %in% 1:cols) {
    # top row
    above = ifelse(i <= cols/2, i + cols/2, i - cols/2)
  } else {
    above = i - cols
  }
  
  # below
  if(i %in% N:(N-cols+1)) {
    # bottom row
    below = ifelse(i %% cols <= cols/2, i + cols/2, i - cols/2)
  } else {
    below = i + cols
  }
  
  neighbours[i, ] <- c(left, right, above, below)
}

# Reduce the cells with missing neighbours
reduce_range_edge <- function(species.range) {
  for(i in which(species.range == 1)) {
    # Rooks case neighbours + 1:
    j <- sum(species.range[neighbours[i, ]] > 0) + 1
    if(j == 5) next # Next; all here
    # No neighbours, 1 neighbour, 2 neigbours, 3 neigbours reduction factors:
    reduction.factor <- c(1/9, 1/6, 1/4, 1/2)[j]
    species.range[i] <- species.range[i] * reduction.factor
  }
  return(species.range)
}





# Reduce ranges and store -------------------------------------------------

# Load maps
current.maps <- readRDS("builds/current.maps.filtered.rds")
present.natural.maps <- readRDS("builds/present.natural.maps.filtered.rds")

# Reduce range edges and save maps
current.maps.edge.lim <- t(apply(current.maps, 1, reduce_range_edge))
current.maps.edge.lim %>%
  as("sparseMatrix") %>% 
  write_rds(file = "builds/current.maps.filtered.edge.lim.rds")
present.natural.maps.edge.lim <- t(apply(present.natural.maps, 1, reduce_range_edge))
present.natural.maps.edge.lim %>%
  as("sparseMatrix") %>% 
  write_rds(file = "builds/present.natural.maps.filtered.edge.lim.rds")




# Check results -----------------------------------------------------------
# 
# # Plot maps with differences
# pr <- par(mfrow = c(2, 2))
# base.map[] <- colSums(present.natural.maps[])
# plot(base.map, main = "present.natural.maps")
# 
# base.map[] <- colSums(present.natural.maps.edge.lim[])
# plot(base.map, main = "present.natural.maps.edge.lim")
# 
# base.map[] <- colSums(current.maps[])
# plot(base.map, main = "current.maps")
# 
# base.map[] <- colSums(current.maps.edge.lim[])
# plot(base.map, main = "current.maps.edge.lim")
# par(pr)
