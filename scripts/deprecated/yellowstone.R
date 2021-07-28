yellowstone.shape <- st_read("Data/yellowstone/ab67.shp")
yellowstone.shape <- st_transform(yellowstone.shape, crs(base.map))
yellowstone.lines <- st_cast(yellowstone.shape, "MULTILINESTRING")

library(stars)
library(fasterize)
stars.base.map <- st_as_stars(base.map)

range <- fasterize(yellowstone.shape, base.map, fun = "first", background = 0)
range.edges <- st_rasterize(yellowstone.lines, stars.base.map, options = "ALL_TOUCHED=TRUE")
range.edges <- as.vector(range.edges$AREA)
range.edges <- ifelse(is.na(range.edges), 0, 1)
range[] <- ifelse(range[] + range.edges, 1, 0)
select <- which(range[] == 1)

range.crop <- crop(current.consumption.map, yellowstone.shape, snap = "out")
range.spdf <- as(range.crop, "SpatialPixelsDataFrame")
range.df <- as_tibble(range.spdf)
ggplot(range.df) +
  geom_tile(aes(x, y, fill = base_map ), col = "grey") +
  geom_sf(data = yellowstone.shape, fill = NA, col = "red")


current.consumption.map[select]

select %in% remove.areas



ggplot(current.consumption.map.df) +
  geom_tile(aes(x = x, y = y, fill = value)) +
  # coord_equal(ylim = range(current.consumption.map.df$y)) +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.map.df$value)) +
  #geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  geom_sf(data = yellowstone.shape, fill = NA, col = "red")


mean(current.consumption.map[s])


yellowstone.df <- readxl::read_xlsx("data/Yellowstone.xlsx")

consumption %>% filter(Binomial.1.2 %in% yellowstone.df$Binomial) %>% pull(Q.plant) %>% sum(.)/10^3




our.mass <- cu.mass[c(7990, 8350)]

s <- which(df$Binomial.1.2 %in% yellowstone.df$Binomial)

sum(10^df$log10density[s] * df$Mass.g[s]/1000)

test <- df %>% right_join(yellowstone.df, by = c("Binomial.1.2" = "Binomial"))

sum(test$Mass.g/1000 * as.numeric(test$Population.size)/8991 , na.rm = T)
