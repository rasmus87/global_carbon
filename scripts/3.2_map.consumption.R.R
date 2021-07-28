# Map consumption
# Run after 3.0_load.data.R
# 27/07-2021 Rasmus Ø Pedersen

# Load libraries
library(viridis)  # better colors for everyone
library(gridExtra)

# Total carbon consumption map [MgC / km2 / year] -----------------------------------
current.consumption.plot <- ggplot(current.consumption.df, aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.df$value)) +
  ggthemes::theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(consumption.df$y))

present.natural.consumption.plot <- ggplot(present.natural.consumption.df, aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.df$value)) +
  ggthemes::theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(consumption.df$y))

# Change in consumption
change.spdf <- as(change, "SpatialPixelsDataFrame")
change.df <- as_tibble(change.spdf)
colnames(change.df) <- c("value", "x", "y")
change.df$period <- "Difference"
change.plot <- ggplot(change.df, aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_gradientn(name = Difference~('%'),
                       na.value = "white",
                       colours = plasma(10)) +
  ggthemes::theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(consumption.df$y))

g11 <- ggplotGrob(current.consumption.plot)
g12 <- ggplotGrob(present.natural.consumption.plot)
g13 <- ggplotGrob(change.plot)
p1 <- gtable_rbind(g11, g12, g13)
arrangeGrob(p1) %>% plot
if(full) {
  ggsave("./output/fig1_carbon_consumption_full.png", p1, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.1)
} else {
  ggsave("./output/fig1_carbon_consumption200.png", p1, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.1)
}




# Fraction carbon consumption map (Carbon consumed / Carbon produced) [%] ----------
frac.npp.cu.consumption.plot <- ggplot(current.npp.use.df %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "hotpink",
                     limits = range(npp.use$value[npp.use$value != 101])) +
  ggthemes::theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(npp.use$y))

frac.npp.pn.consumption.plot <- ggplot(present.natural.npp.use.df %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "hotpink",
                     limits = range(npp.use$value[npp.use$value != 101])) +
  ggthemes::theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(npp.use$y))

# Difference in consumption
change.pct.spdf <- as(change.pct.point, "SpatialPixelsDataFrame")
change.pct.df <- as_tibble(change.pct.spdf)
colnames(change.pct.df) <- c("value", "x", "y")
change.pct.df$period <- "Percentage point difference"
pct.pt.diffrence.plot <- ggplot(change.pct.df %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_gradientn(name = Difference~('%-point'),
                       na.value = "hotpink",
                       colours = plasma(10)) +
  ggthemes::theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(npp.use$y))

g21 <- ggplotGrob(frac.npp.cu.consumption.plot)
g22 <- ggplotGrob(frac.npp.pn.consumption.plot)
g23 <- ggplotGrob(pct.pt.diffrence.plot)
p2 <- gtable_rbind(g21, g22, g23)
arrangeGrob(p2) %>% plot
if(full) {
  ggsave("./output/fig2_fraction_npp_consumed_full.png", p2, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.1)
} else {
  ggsave("./output/fig2_fraction_npp_consumed200.png", p2, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.1)
}

