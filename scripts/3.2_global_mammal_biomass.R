# Calculate global mammal biomass
# Does not need to be run for 3.2+
# 27/07-2021 Rasmus Ø Pedersen



# Load and sample density data --------------------------------------------

land <- raster("builds/land.tif")
land.v <- land[]
land.i <- which(!is.na(land.v))

cell.area <- prod(res(base.map)) * 10^-6 # km^2
wet_to_carbon <- .50 * .30 # carbon.mass / wet.mass


# Naive across summary cells
mass.df %>%
  group_by(period) %>% 
  summarise(wet.mass.Mg.km2 = sum(value),
            carbon.mass.Pg.total = wet.mass.Mg.km2 * wet_to_carbon * cell.area * 10^-9)


# Find indexes of removed land in the land only maps
rm.land.index <- which(land.i %in% remove.areas)

# Actual across all maps
cu.mass.maps.land <- read_rds("builds/cu.mass.maps.land.rds") # [Mg / km2]
pn.mass.maps.land <- read_rds("builds/pn.mass.maps.land.rds") # [Mg / km2]

cu.mass.PgC <- tibble(mass.PgC = colSums(cu.mass.maps.land[-rm.land.index, ]) * wet_to_carbon * cell.area * 10^-9, # [PgC]
                      period = "Current")
pn.mass.PgC <- tibble(mass.PgC = colSums(pn.mass.maps.land[-rm.land.index, ]) * wet_to_carbon * cell.area * 10^-9, # [PgC]
                      period = "Present natural")
mass.PgC <- bind_rows(cu.mass.PgC, pn.mass.PgC)

mass.PgC %>%
  group_by(period) %>% 
  summarise(geo.mean = exp(mean(log(mass.PgC))),
            geo.sd = exp(sd(log(mass.PgC))),
            q025 = quantile(mass.PgC, probs = .025),
            q975 = quantile(mass.PgC, probs = .975))





# 
# 
# # Global mammal biomass in Carbon -----------------------------------------
# 
# # For comparison with:
# # Bar-On, Y. M., Phillips, R., & Milo, R. (2018). The biomass distribution on
# # Earth. Proceedings of the National Academy of Sciences of the United States of
# # America, 115(25), 6506–6511. https://doi.org/10.1073/pnas.1711842115
# # Following their methods dry weight is 30 % and carbon is 50 % 
# # so kgC = 0.3 * 0.5 kg wet weight = 0.15 * kg wet weight in total
# # [Pg] = [GtC] = 0.15 * BM
# 
# # Remove cells as selected in 3.0
# current.maps.reduced <- current.maps[, -remove.areas]
# present.natural.maps.reduced <- present.natural.maps[, -remove.areas]
# 
# # Megafauna only: # Definition as https://www.pnas.org/doi/10.1073/pnas.0801918105#sec-1
# # Megafauna only test for comparison to Barnoky (2008)
# mf <- df %>% filter(Mass.g >= 44000) %>% pull(Binomial.1.2)
# 
# n.cells.cu <- rowSums(current.maps.reduced[], na.rm = TRUE)
# n.cells.cu[!names(n.cells.cu) %in% mf] <- 0 # Megafauna only test for comparison to Barnoky (2008)
# n.cells.pn <- rowSums(present.natural.maps.reduced[], na.rm = TRUE)
# n.cells.pn[!names(n.cells.pn) %in% mf] <- 0 # Megafauna only test for comparison to Barnoky (2008)
# base.map <- raster("builds/base_map.tif")
# # dens [1/km2] * mass [g] * 10^-15 Pg/g * cell-resolution [m^2] * 10^-6 (km/m)^2
# # Pg
# 
# cell.area <- prod(res(base.map)) * 10^-6 # km^2
# wet_to_carbon <- .50 * .30 # carbon.mass / wet.mass
# mass.Pg <- df$Mass.g * 10^-15 # Pg (or Gt)
# density <- 10^df$log10density # km^-2
# mass.pr.cell <- density * mass.Pg * wet_to_carbon * cell.area # Pg Carbon
# 
# 
# density.samples <- 10^log10dens.samples # km^-2
# mass.pr.cell.samples <- density.samples * mass.Pg * wet_to_carbon * cell.area # Pg Carbon
# mass.pr.cell.summary <- apply(mass.pr.cell.samples, 1, function(x) c(quantile(x, 0.025), median = median(x), quantile(x, 0.975)))
# mass.cu <- colSums(t(mass.pr.cell.summary) * n.cells.cu)
# mass.cu.mean <- colSums(t(mass.pr.cell.summary[2, ]) * n.cells.cu)
# mass.pn <- colSums(t(mass.pr.cell.summary) * n.cells.pn)
# mass.cu.r <- colSums(t(mass.pr.cell.summary) * n.cells.cu * !(df$IUCN.Status.1.2 %in% c("EP", "EX", "EW", "CR", "EN", "VU")))
# 
# paste0("Present natural mass: ", signif(mass.pn[2], 2), " PgC (95%-CI: ", signif(mass.pn[1], 2), "-", signif(mass.pn[3], 2), ")")
# paste0("Current potential mass: ", signif(mass.cu[2], 2), " PgC (95%-CI: ", signif(mass.cu[1], 2), "-", signif(mass.cu[3], 2), ")")
# paste0("Current without CR/EN/VU mass: ", signif(mass.cu.r[2], 2), " PgC (95%-CI: ", signif(mass.cu.r[1], 2), "-", signif(mass.cu.r[3], 2), ")")
# 
# # Wet in x10^11
# density.samples <- 10^log10dens.samples # km^-2
# mass.pr.cell.samples <- density.samples * mass.Pg * cell.area * 10 # 10^11 g Carbon
# mass.pr.cell.summary <- apply(mass.pr.cell.samples, 1, function(x) c(quantile(x, 0.025), geo.mean = exp(mean(log(x))), quantile(x, 0.975)))
# mass.pn <- colSums(t(mass.pr.cell.summary) * n.cells.pn)
# paste0("Present natural mass: ", signif(mass.pn[2], 2), " x10^11 kg wet (95%-CI: ", signif(mass.pn[1], 2), "-", signif(mass.pn[3], 2), ")")
# 
# # Proboscidea only
# density.samples <- 10^log10dens.samples # km^-2
# density.samples <- density.samples * (df$Order.1.2 == "Proboscidea")
# mass.pr.cell.samples <- density.samples * mass.Pg * wet_to_carbon * cell.area # Pg Carbon
# mass.pr.cell.summary <- apply(mass.pr.cell.samples, 1, function(x) c(quantile(x, 0.025), median = median(x), quantile(x, 0.975)))
# mass.cu.pro <- colSums(t(mass.pr.cell.summary) * n.cells.cu)
# mass.pn.pro <- colSums(t(mass.pr.cell.summary) * n.cells.pn)
# 
# paste0("Current proboscidean mass: ", signif(mass.cu.pro[2], 2), " PgC (95%-CI: ", signif(mass.cu.pro[1], 2), "-", signif(mass.cu.pro[3], 2), ")")
# paste0("Present natural proboscidean mass: ", signif(mass.pn.pro[2], 2), " PgC (95%-CI: ", signif(mass.pn.pro[1], 2), "-", signif(mass.pn.pro[3], 2), ")")
# 
# paste0("Current proboscidean pct: ", signif(mass.cu.pro[2]/mass.cu[2]*100, 3), " % (95%-CI: ", signif(mass.cu.pro[1]/mass.cu[1]*100, 3), "-", signif(mass.cu.pro[3]/mass.cu[3]*100, 3), ")")
# paste0("Present natural proboscidean pct: ", signif(mass.pn.pro[2]/mass.pn[2]*100, 3), " % (95%-CI: ", signif(mass.pn.pro[1]/mass.pn[1]*100, 3), "-", signif(mass.pn.pro[3]/mass.pn[3]*100, 3), ")")
# 
# # 17 086 animals 2015
# #https://koedoe.co.za/index.php/koedoe/article/view/1427/2070#:~:text=A%20minimum%20of%2017%20086,management%20era%20ending%20in%201994.
# kruger.loxodonta <- 17086
# k.size <- 19485 #km2
# kruger.density <- kruger.loxodonta/k.size
# krug <- tibble(src = "---Empircal Kruger NatPark---", Binomial.1.2 = "Loxodonta_africana", dens.pr.km2 = kruger.density)
# 
# p <- which(df$Order.1.2 == "Proboscidea")
# prob.pantheria <- tibble(src = "Imputed based on PanTHERIA", Binomial.1.2 = df$Binomial.1.2[p], dens.pr.km2 = round(density[p],2)) %>% arrange(dens.pr.km2)
# density.alt <- 10^apply(log10dens.samples.alt, 1, median)
# prob.alt <- tibble(src = "Imputed based on TetraDENS", Binomial.1.2 = df$Binomial.1.2[p], dens.pr.km2 = round(density.alt[p],2)) %>% arrange(dens.pr.km2)
# 
# pant <- read_csv("../mammal_density/builds/imputation_dataset_PanTHERIA.csv")
# pant <- pant %>% filter(Order.1.2 == "Proboscidea") %>% transmute(src = "PanTHERIA", Binomial.1.2, dens.pr.km2 = 10^log10density)
# tetra <- read_csv("../mammal_density/builds/imputation_dataset_PanTHERIA_TetraDENSITY.csv")
# tetra <- tetra %>% filter(Order.1.2 == "Proboscidea") %>% transmute(src = "TetraDENSITY", Binomial.1.2, dens.pr.km2 = 10^log10density)
# tetra.m <- tetra %>% group_by(Binomial.1.2) %>% summarise(dens.pr.km2 = median(dens.pr.km2)) %>% mutate(src = "TetraDENSITY median")
# tetra.025 <- tetra %>% group_by(Binomial.1.2) %>% summarise(dens.pr.km2 = quantile(dens.pr.km2, 0.025)) %>% mutate(src = "TetraDENSITY Quantile.025")
# tetra.975 <- tetra %>% group_by(Binomial.1.2) %>% summarise(dens.pr.km2 = quantile(dens.pr.km2, 0.975)) %>% mutate(src = "TetraDENSITY Quantile.975")
# 
# tab <- bind_rows(krug, prob.pantheria, prob.alt, pant, tetra.m, tetra.025, tetra.975) %>%  arrange(dens.pr.km2) %>% print(n = 100)
# write_excel_csv(tab, "output/Proboscidea_density.csv")
# 
# # The paper says:
# # Current: 0.003 Pg (livestock = 0.1)
# # Present natural: 0.02 Pg
