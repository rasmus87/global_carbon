# Run after 3.0_load.data.R

detach(package:cowplot)

### CACULATE TOTALT GLOBAL CONSUMPTION >>>
# Units:  
# [MgC / (km2 * year) * m2 * km2 / 10^6 m2 * 10^6 g / Mg * Pg / 10^15 g] = 
# [PgC / year]
tot.current <- sum(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat <- sum(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
diff <- tot.pres.nat - tot.current

tot.current.med <- median(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat.med <- median(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
tot.current.mean <- mean(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat.mean <- mean(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15

tot.current.lw <- sum(current.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat.lw <- sum(present.natural.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T) * 10^6 / 10^15
diff.lw <- tot.pres.nat.lw - tot.current.lw

tot.current.hi <- sum(current.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat.hi <- sum(present.natural.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T) * 10^6 / 10^15
diff.hi <- tot.pres.nat.hi - tot.current.hi

paste0("Current consumption: ", signif(tot.current, 2), " Pg Carbon / year (95%-CI: ", signif(tot.current.lw, 2), "-", signif(tot.current.hi, 2) ,")")
paste0("Present natural consumption: ", signif(tot.pres.nat, 2), " Pg Carbon / year (95%-CI: ", signif(tot.pres.nat.lw, 2), "-", signif(tot.pres.nat.hi, 2) ,")")
paste0("Difference: ", signif(diff, 2), " Pg Carbon / year (95%-CI: ", signif(diff.lw, 2), "-", signif(diff.hi, 2) ,")")
### CACULATE TOTALT GLOBAL CONSUMPTION |||

#### CONSUMPTION OF NPP (%) >>>
# Of total (Pg)
tot.npp <- sum(npp[] * prod(res(npp))/10^6, na.rm = T)*10^6/10^15
tot.npp
# Percentage released (Doughty says 2.2-5.3)
(tot.pres.nat - tot.current)/tot.npp * 100
(tot.pres.nat.lw - tot.current.lw)/tot.npp * 100
(tot.pres.nat.hi - tot.current.hi)/tot.npp * 100
#### CONSUMPTION OF NPP (%) |||



### Carbon consumption map [MgC / km2 / year] >>>
current.consumption.plot <- ggplot(current.consumption.df, aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.df$y)) +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.map.df$value)) +
  theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

present.natural.consumption.plot <- ggplot(present.natural.consumption.df, aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.df$y)) +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.map.df$value)) +
  theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold")) + 
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Change in consumption
change.spdf <- as(change, "SpatialPixelsDataFrame")
change.df <- as_tibble(change.spdf)
colnames(change.df) <- c("value", "x", "y")
change.df$period <- "Difference"
change.plot <- ggplot(change.df, aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.df$y)) +
  scale_fill_gradientn(name = Difference~('%'),
                       na.value = "white",
                       colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

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
### Carbon consumption map [MgC / km2 / year] |||



### Carbon consumption map (Carbon consumed / Carbon produced) [%] >>>
# Map consumption of carbon production
frac.npp.cu.consumption.plot <- ggplot(cu.npp.use %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "hotpink",
                     limits = range(npp.use$value[npp.use$value != 101])) +
  theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

frac.npp.pn.consumption.plot <- ggplot(pn.npp.use %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "hotpink",
                     limits = range(npp.use$value[npp.use$value != 101])) +
  theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Difference in consumption
change.pct.spdf <- as(change.pct.point, "SpatialPixelsDataFrame")
change.pct.df <- as_tibble(change.pct.spdf)
colnames(change.pct.df) <- c("value", "x", "y")
change.pct.df$period <- "Percentage point difference"
pct.pt.diffrence.plot <- ggplot(change.pct.df %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(period ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_gradientn(name = Difference~('%-point'),
                       na.value = "hotpink",
                       colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

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
# ### Carbon consumption map (Carbon consumed / Carbon produced) [%] |||
