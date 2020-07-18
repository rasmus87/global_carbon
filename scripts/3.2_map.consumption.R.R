# Run after 3.0_load.data.R

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

paste0("Current consumption: ", round(tot.current), " Pg Carbon / year (95%-CI: ", round(tot.current.lw), "-", round(tot.current.hi) ,")")
paste0("Present natural consumption: ", round(tot.pres.nat), " Pg Carbon / year (95%-CI: ", round(tot.pres.nat.lw), "-", round(tot.pres.nat.hi) ,")")
paste0("Difference: ", round(diff), " Pg Carbon / year (95%-CI: ", round(diff.lw), "-", round(diff.hi) ,")")
### CACULATE TOTALT GLOBAL CONSUMPTION |||


### Carbon consumption map [MgC / km2 / year]
library(rworldmap)
library(maptools)
newmap <- getMap(resolution = "low")
newmap <- unionSpatialPolygons(newmap, rep(1, nrow(newmap)))
newmap <- spTransform(newmap, crs(current.consumption.map))
newmap <- fortify(newmap)

current.consumption.map.spdf <- as(current.consumption.map, "SpatialPixelsDataFrame") # [MgC / km2 / year]
current.consumption.map.df <- as_tibble(current.consumption.map.spdf)
colnames(current.consumption.map.df) <- c("value", "x", "y")
current.consumption.map.df <- current.consumption.map.df %>% mutate(time = "Current")
present.natural.consumption.map.spdf <- as(present.natural.consumption.map, "SpatialPixelsDataFrame")
present.natural.consumption.map.df <- as_tibble(present.natural.consumption.map.spdf)
colnames(present.natural.consumption.map.df) <- c("value", "x", "y")
present.natural.consumption.map.df <- present.natural.consumption.map.df %>% mutate(time = "Present natural")
consumption.map.df <- bind_rows(current.consumption.map.df, present.natural.consumption.map.df)

current.consumption.plot <- ggplot(current.consumption.map.df, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.map.df$y)) +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.map.df$value)) +
  theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

present.natural.consumption.plot <- ggplot(present.natural.consumption.map.df, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.map.df$y)) +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.map.df$value)) +
  theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold")) + 
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Change in consumption
change <- (current.consumption.map/present.natural.consumption.map - 1)*100
change[change > 0] <- 0
change.spdf <- as(change, "SpatialPixelsDataFrame")
change.df <- as_tibble(change.spdf)
colnames(change.df) <- c("value", "x", "y")
change.df$time <- "Difference"
change.plot <- ggplot(change.df, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.map.df$y)) +
  scale_fill_gradientn(name = Difference~('%'),
                       na.value = "white",
                       colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

g11 <- ggplotGrob(current.consumption.plot)
g12 <- ggplotGrob(present.natural.consumption.plot)
g13 <- ggplotGrob(change.plot)
p1 <- gtable_rbind(g11, g12, g13)
arrangeGrob(p1) %>% plot
ggsave("./output/fig1_carbon_consumption.png", p1, width = 20, height = 23, units = "cm")
### Carbon consumption map [MgC / km2 / year] |||

# Load Modis data of NPP
# Modis data are based on leaf area and solar energy input
# It is total NPP, and before _any_ consumption 
# (other than what has been subtracted for respiration)
npp <- raster("../large_datasets/MOD17A3/NPP_projected_resampled_scaled_gCarbon_m2_yr.tif") # [gC / m2 / yr]
npp.cut <- raster("data/Q95_extreme_sd_npp1.tif")
npp[which(npp.cut[] == 1)] <- NA
npp[] <- npp[] * 1000^2 / 10^6 # Mg Carbon / km2 / yr


#### CONSUMPTION OF NPP (%) >>>
# Of total (Pg)
tot.npp <- sum(npp[] * prod(res(npp))/10^6, na.rm = T)*10^6/10^15
tot.npp
# Percentage released (Doughty says 2.2-5.3)
(tot.pres.nat - tot.current)/tot.npp * 100
(tot.pres.nat.lw - tot.current.lw)/tot.npp * 100
(tot.pres.nat.hi - tot.current.hi)/tot.npp * 100
#### CONSUMPTION OF NPP (%) |||


### Carbon consumption map (Carbon consumed / Carbon produced) [%] >>>
# Find the fraction and correct from KgC to MgC
npp[which(npp[] == 0)] <- NA
current.npp.use <- (current.consumption.map)/npp * 100
present.natural.npp.use <- (present.natural.consumption.map)/npp * 100
current.npp.use.org <- current.npp.use
present.natural.npp.use.org <- present.natural.npp.use

# Truncate fractions above 100 %
current.npp.use[current.npp.use >= 100.5] <- 101
present.natural.npp.use[present.natural.npp.use >= 100.5] <- 101

# Turn maps into dataframes for plotting
current.npp.use.spdf <- as(current.npp.use, "SpatialPixelsDataFrame")
current.npp.use.df <- as_tibble(current.npp.use.spdf)
colnames(current.npp.use.df) <- c("value", "x", "y")
present.natural.npp.use.spdf <- as(present.natural.npp.use, "SpatialPixelsDataFrame")
present.natural.npp.use.df <- as_tibble(present.natural.npp.use.spdf)
colnames(present.natural.npp.use.df) <- c("value", "x", "y")

# Combine datasets
pn.npp.use <- present.natural.npp.use.df %>% mutate(time = "Present natural")
cu.npp.use <- current.npp.use.df %>% mutate(time = "Current")
npp.use <- bind_rows(pn.npp.use, cu.npp.use)
npp.use$time <- as.factor(npp.use$time)
npp.use$time <- fct_relevel(npp.use$time, "Present natural")

# Map consumption of carbon production
frac.npp.cu.consumption.plot <- ggplot(cu.npp.use, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "grey",
                     limits = range(npp.use$value)) +
  theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

frac.npp.pn.consumption.plot <- ggplot(pn.npp.use, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "white",
                     limits = range(npp.use$value)) +
  theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Difference in consumption
change.pct <- current.npp.use - present.natural.npp.use
change.pct[change.pct > 0] <- 0
change.pct.spdf <- as(change.pct, "SpatialPixelsDataFrame")
change.pct.df <- as_tibble(change.pct.spdf)
colnames(change.pct.df) <- c("value", "x", "y")
change.pct.df$time <- "Percentage point difference"
pct.pt.diffrence.plot <- ggplot(change.pct.df, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_gradientn(name = Difference~('%-point'),
                       na.value = "white",
                       colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

g21 <- ggplotGrob(frac.npp.cu.consumption.plot)
g22 <- ggplotGrob(frac.npp.pn.consumption.plot)
g23 <- ggplotGrob(pct.pt.diffrence.plot)
p2 <- gtable_rbind(g21, g22, g23)
# arrangeGrob(p2) %>% plot
# ggsave("./output/fig2_fraction_npp_consumed.png", p2, width = 20, height = 23, units = "cm")

# Make a supplementary map with truncated percentages shown in pink
npp.cu.hot.pink <- ggplot(cu.npp.use %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "hotpink",
                     limits = range(npp.use$value)) +
  theme_map() +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

npp.pn.hot.pink <- ggplot(pn.npp.use %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "hotpink",
                     limits = range(npp.use$value)) +
  theme_map() +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Difference in consumption
change.pct <- current.npp.use - present.natural.npp.use
change.pct[] <- ifelse(current.npp.use[] == 101 | present.natural.npp.use[] == 101 , 101, change.pct[])
# change.pct[change.pct > 0] <- 0
change.pct.spdf <- as(change.pct, "SpatialPixelsDataFrame")
change.pct.df <- as_tibble(change.pct.spdf)
colnames(change.pct.df) <- c("value", "x", "y")
change.pct.df$time <- "Percentage point difference"
pct.pt.diffrence.hot.pinkt <- ggplot(change.pct.df %>% mutate(value = na_if(value, 101)), aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_gradientn(name = Difference~('%-point'),
                       na.value = "hotpink",
                       colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)


gS61 <- ggplotGrob(npp.cu.hot.pink)
gS62 <- ggplotGrob(npp.pn.hot.pink)
gS63 <- ggplotGrob(pct.pt.diffrence.hot.pinkt)
# pS6 <- gtable_rbind(gS61, gS62)
pS6 <- gtable_rbind(gS61, gS62, gS63)
arrangeGrob(pS6) %>% plot
ggsave("./output/fig.S6.npp.consumption.trunc.pink.png", pS6, width = 20, height = 15, units = "cm")
### Carbon consumption map (Carbon consumed / Carbon produced) [%] |||
