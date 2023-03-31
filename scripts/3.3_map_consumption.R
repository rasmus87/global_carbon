# Map consumption
# Run after 3.0_load.data.R
# 29/07-2021 Rasmus Ø Pedersen

library(grid)

# Total carbon consumption map [MgC / km2 / year] -----------------------------------
# Current
current.consumption.plot <- ggplot(current.consumption.df, aes(x = x, y = y, fill = value)) +
  # facet_grid(period ~ .) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.df$value),
                     option = "turbo") +
  theme_map() +
  labs(subtitle = "a) Current") +
  theme(plot.subtitle = element_text(face = "bold", size = 10)) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)

# Present natural
present.natural.consumption.plot <- ggplot(present.natural.consumption.df, aes(x = x, y = y, fill = value)) +
  # facet_grid(period ~ .) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.df$value),
                     option = "turbo") +
  theme_map() +
  labs(subtitle = "b) Present-natural") +
  theme(plot.subtitle = element_text(face = "bold", size = 10)) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)

# Change in consumption
change.plot <- ggplot(change.df, aes(x = x, y = y, fill = value)) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = Difference~('%'),
                     na.value = "white",
                     option = "turbo",
                     direction = -1) +
  theme_map() +
  labs(subtitle = "c) Change (a/b - 1)") +
  theme(plot.subtitle = element_text(face = "bold", size = 10)) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)

g11 <- ggplotGrob(current.consumption.plot)
g12 <- ggplotGrob(present.natural.consumption.plot)
g13 <- ggplotGrob(change.plot)
line <- linesGrob(y = 0, gp = gpar(lty = 2))
p1 <- arrangeGrob(g11, g12, line, g13, nrow = 4, heights = c(1, 1, .01, 1))
# p1 <- gtable_rbind(g11, g12, g13)
arrangeGrob(p1) %>% plot
if(full) {
  ggsave("./output/fig1_carbon_consumption_full.png", p1, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.1)
} else {
  ggsave("./output/fig1_carbon_consumption200.png", p1, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.1)
}




# Fraction carbon consumption map (Carbon consumed / Carbon produced) [%] ----------
# Current
frac.npp.cu.consumption.plot <- ggplot(current.npp.use.df %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "hotpink",
                     limits = range(npp.use$value[npp.use$value != 101]),
                     option = "turbo") +
  theme_map() +
  labs(subtitle = "a) Current") +
  theme(plot.subtitle = element_text(face = "bold", size = 10)) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)

# Present natural
frac.npp.pn.consumption.plot <- ggplot(present.natural.npp.use.df %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "hotpink",
                     limits = range(npp.use$value[npp.use$value != 101]),
                     option = "turbo") +
  theme_map() +
  labs(subtitle = "b) Present-natural") +
  theme(plot.subtitle = element_text(face = "bold", size = 10)) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)

# Difference in consumption
pct.pt.diffrence.plot <- ggplot(change.pct.df %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = Difference~('%-point'),
                     na.value = "hotpink",
                     option = "turbo",
                     direction = -1) +
  theme_map() +
  labs(subtitle = "c) Change (a - b)") +
  theme(plot.subtitle = element_text(face = "bold", size = 10)) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)

g21 <- ggplotGrob(frac.npp.cu.consumption.plot)
g22 <- ggplotGrob(frac.npp.pn.consumption.plot)
g23 <- ggplotGrob(pct.pt.diffrence.plot)
line <- linesGrob(y = 0, gp = gpar(lty = 2))
p2 <- arrangeGrob(g21, g22, line, g23, nrow = 4, heights = c(1, 1, .01, 1))
# p2 <- gtable_rbind(g21, g22, g23)
arrangeGrob(p2) %>% plot
if(full) {
  ggsave("./output/fig2_fraction_npp_consumed_full.png", p2, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.1)
} else {
  ggsave("./output/fig2_fraction_npp_consumed200.png", p2, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.1)
}






# Appendix uncertainty ----------
# Current
cu.consumption.geo.sd.plot <- ggplot(cu.consumption.geo.sd.df, aes(x = x, y = y, fill = value)) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = "Goemetric SD factor",
                     na.value = "hotpink",
                     limits = range(0, 2),
                     option = "turbo") +
  theme_map() +
  labs(subtitle = "a) Current") +
  theme(plot.subtitle = element_text(face = "bold", size = 10)) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)

# Present natural
pn.consumption.geo.sd.plot <- ggplot(pn.consumption.geo.sd.df, aes(x = x, y = y, fill = value)) +
  geom_tile(data = land.df, aes(fill = NULL, period = NULL), fill = "grey90") +
  geom_tile() +
  scale_fill_viridis(name = "Goemetric SD factor",
                     na.value = "hotpink",
                     limits = range(0, 2),
                     option = "turbo") +
  theme_map() +
  labs(subtitle = "b) Present-natural") +
  theme(plot.subtitle = element_text(face = "bold", size = 10)) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)


g21 <- ggplotGrob(cu.consumption.geo.sd.plot)
g22 <- ggplotGrob(pn.consumption.geo.sd.plot)
p3 <- arrangeGrob(g21, g22, nrow = 2, heights = c(1, 1))
arrangeGrob(p3) %>% plot
ggsave("./output/appendix_consumption.geo.sd.png", p3, width = 183, height = 140, units = "mm", dpi = 600, scale = 1.1)

