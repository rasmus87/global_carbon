# Load the filtered maps, and halve the range outline cells and save again

# Load libs
library(raster)

# Load base map
base.map <- raster("builds/base_map.tif")


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
    reduction.factor <- c(1/9, 1/6, 1/4, 1/2)[j]
    species.range[i] <- species.range[i] * reduction.factor
  }
  return(species.range)
}

# Load maps
current.maps <- readRDS("builds/current.maps.filtered.rds")
present.natural.maps <- readRDS("builds/present.natural.maps.filtered.rds")

# Reduce range edges and save maps
current.maps.edge.lim <- t(apply(current.maps, 1, reduce_range_edge))
saveRDS(current.maps.edge.lim, file = "builds/current.maps.filtered.edge.lim.rds")
present.natural.maps.edge.lim <- t(apply(present.natural.maps, 1, reduce_range_edge))
saveRDS(present.natural.maps.edge.lim, file = "builds/present.natural.maps.filtered.edge.lim.rds")

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
