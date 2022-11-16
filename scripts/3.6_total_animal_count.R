# Calculate number of animals
# Run after 3.0_load.data.R
# 8/09-2022 Rasmus Ø Pedersen


# Remove cells as selected in 3.0
current.maps.reduced <- current.maps[, -remove.areas]
present.natural.maps.reduced <- present.natural.maps[, -remove.areas]

n.cells.cu <- rowSums(current.maps.reduced[], na.rm = TRUE)
n.cells.pn <- rowSums(present.natural.maps.reduced[], na.rm = TRUE)
base.map <- raster("builds/base_map.tif")

cell.area <- prod(res(base.map)) * 10^-6 # km^2
density <- 10^df$log10density # km^-2

count <- df %>% 
  mutate(count.cu = density * n.cells.cu * cell.area,
         count.pn = density * n.cells.pn * cell.area)

count %>% 
  filter(Mass.g >= 10^6) %>% 
  summarise(count.cu.million = sum(count.cu)/10^6,
            count.pn.million = sum(count.pn)/10^6)


# Second method

# Load and sample density data --------------------------------------------

# Set seed to get consistent results:
set.seed(42)

# Set samples
n.samples <- 1000

# Load population density posterior distribution
dens <- read_csv("../mammal_density/builds/densities_post.pred.csv") # log10 individuals / km2
# Make sure all species are there
all(df$Binomial.1.2 %in% names(dens))
# Subset
dens <- dens[df$Binomial.1.2]
# Make sure alignment is right
stopifnot(all.equal(names(dens), df$Binomial.1.2))
# Sample
log10dens.samples <- t(sample_n(dens, n.samples))

mega <- which(df$Mass.g >= 10^6)

density.samples <- 10^log10dens.samples[mega, ] # km^-2
count.pr.cell.samples <- density.samples * cell.area # animals per cell
count.samples.cu <- count.pr.cell.samples * n.cells.cu[mega] * 10^-6 # million animals
count.samples.pn <- count.pr.cell.samples * n.cells.pn[mega] * 10^-6 # million animals

total.cu <- colSums(count.samples.cu)
mean(total.cu)
count.cu <- quantile(total.cu, c(0.025, .5, 0.975))
count.cu
total.cu %>% density %>% plot

total.pn <- colSums(count.samples.pn)
mean(total.pn)
count.pn <- quantile(total.pn, c(0.025, .5, 0.975))
count.pn
total.pn %>% density %>% plot

total.cu.r <- colSums(count.samples.cu * !(df$IUCN.Status.1.2[mega] %in% c("EP", "EX", "EW", "CR", "EN", "VU")))
count.cu.r <- quantile(total.cu.r, c(0.025, .5, 0.975))
count.cu.r
total.cu.r %>% density %>% plot

paste0("Present natural count: ", signif(count.pn[2], 2), " million animals (95%-CI: ", signif(count.pn[1], 2), "-", signif(count.pn[3], 2), ")")
paste0("Current potential count: ", signif(count.cu[2], 2), " million animals (95%-CI: ", signif(count.cu[1], 2), "-", signif(count.cu[3], 2), ")")
paste0("Current without CR/EN/VU count: ", signif(count.cu.r[2], 2), " million animals (95%-CI: ", signif(count.cu.r[1], 2), "-", signif(count.cu.r[3], 2), ")")



median.samples <- apply(density.samples, 1, median)

df %>% 
  filter(Mass.g >= 10^6) %>% 
  mutate(count.cu = rowMeans(density.samples) * n.cells.cu[mega] * cell.area,
         count.pn = rowMeans(density.samples) * n.cells.pn[mega] * cell.area) %>% 
  summarise(count.cu.million = sum(count.cu)/10^6,
            count.pn.million = sum(count.pn)/10^6)

df %>% 
  filter(Mass.g >= 10^6) %>% 
  mutate(count.cu = median.samples * n.cells.cu[mega] * cell.area,
         count.pn = median.samples * n.cells.pn[mega] * cell.area) %>% 
  summarise(count.cu.million = sum(count.cu)/10^6,
            count.pn.million = sum(count.pn)/10^6)

