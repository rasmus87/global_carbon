library(tidyverse)
library(tictoc)

# Set seed to get consistent results:
set.seed(42)

# Load data
df <- read_csv("builds/data.csv", col_types = cols())

# We sample the distributions instead
n.samples <- 1000
timestamp()
tic()
# ... and density distributions
dens <- read_csv("../mammal_density/builds/3_densities_post.pred.csv") # individuals / km2
all.equal(names(dens), df$Binomial.1.2)
log10dens.samples <- t(sample_n(dens, n.samples))

dens.alt <- read_csv("../mammal_density/builds/3_densities_post.pred.alt.csv") # individuals / km2
all.equal(names(dens), df$Binomial.1.2)
log10dens.samples.alt <- t(sample_n(dens.alt, n.samples))
toc()


# Global mammal biosmass in Carbon >>>
# For comparison with:
# Bar-On, Y. M., Phillips, R., & Milo, R. (2018). The biomass distribution on
# Earth. Proceedings of the National Academy of Sciences of the United States of
# America, 115(25), 6506–6511. https://doi.org/10.1073/pnas.1711842115
# Following their methods dry weight is 30 % and carbon is 50 % 
# so kgC = 0.3 * 0.5 kg wet weight = 0.15 * kg wet weight in total
# [Pg] = [GtC] = 0.15 * BM * 

library(raster)
load("builds/current.maps.filtered.edge.lim.RData")
load("builds/present.natural.maps.filtered.edge.lim.RData")
n.cells.cu <- rowSums(current.maps.edge.lim[])
n.cells.pn <- rowSums(present.natural.maps.edge.lim[])
base.map <- raster("builds/base_map.tif")
# dens [1/km2] * mass [g] * 10^-15 Pg/g * cell-resolution [m^2] * 10^-6 (km/m)^2
# Pg

cell.area <- prod(res(base.map)) * 10^-6 # km^2
wet_to_carbon <- .50 * .30 # carbon.mass / wet.mass
mass.Pg <- df$Mass.g * 10^-15 # Pg (or Gt)
density <- 10^df$log10density # km^-2
mass.pr.cell <- density * mass.Pg * wet_to_carbon * cell.area # Pg Carbon

order.tab <- df %>% 
  mutate(mass = mass.pr.cell * n.cells.cu, massT = sum(mass)) %>% 
  group_by(Order.1.2) %>% 
  summarise(mass.Pg = sum(mass), pct = mass.Pg/massT[1]*100, n.species = n()) %>% 
  print(n = 100)
order.tab
write_excel_csv(order.tab, "output/Current_total_mass_by_order.csv")

density.samples <- 10^log10dens.samples # km^-2
mass.pr.cell.samples <- density.samples * mass.Pg * wet_to_carbon * cell.area # Pg Carbon
mass.pr.cell.summary <- apply(mass.pr.cell.samples, 1, function(x) c(quantile(x, 0.025), median = median(x), quantile(x, 0.975)))
mass.cu <- colSums(t(mass.pr.cell.summary) * n.cells.cu)
mass.pn <- colSums(t(mass.pr.cell.summary) * n.cells.pn)
mass.cu.r <- colSums(t(mass.pr.cell.summary) * n.cells.cu * !(df$IUCN.Status.1.2 %in% c("EP", "EX", "EW", "CR", "EN", "VU")))

paste0("Present natural mass: ", signif(mass.pn[2], 2), " PgC (95%-CI: ", signif(mass.pn[1], 2), "-", signif(mass.pn[3], 2), ")")
paste0("Current potential mass: ", signif(mass.cu[2], 2), " PgC (95%-CI: ", signif(mass.cu[1], 2), "-", signif(mass.cu[3], 2), ")")
paste0("Current without CR/EN/VU mass: ", signif(mass.cu.r[2], 2), " PgC (95%-CI: ", signif(mass.cu.r[1], 2), "-", signif(mass.cu.r[3], 2), ")")

density.samples.alt <- 10^log10dens.samples.alt # km^-2
mass.pr.cell.samples.alt <- density.samples.alt * mass.Pg * wet_to_carbon * cell.area # Pg Carbon
mass.pr.cell.summary.alt <- apply(mass.pr.cell.samples.alt, 1, function(x) c(quantile(x, 0.025), median = median(x), quantile(x, 0.975)))
mass.cu.alt <- colSums(t(mass.pr.cell.summary.alt) * n.cells.cu)
mass.pn.alt <- colSums(t(mass.pr.cell.summary.alt) * n.cells.pn)
mass.cu.r.alt <- colSums(t(mass.pr.cell.summary.alt) * n.cells.cu * !(df$IUCN.Status.1.2 %in% c("EP", "EX", "EW", "CR", "EN", "VU")))

paste0("Present natural mass: ", signif(mass.pn.alt[2], 2), " PgC (95%-CI: ", signif(mass.pn.alt[1], 2), "-", signif(mass.pn.alt[3], 2), ")")
paste0("Current potential mass: ", signif(mass.cu.alt[2], 2), " PgC (95%-CI: ", signif(mass.cu.alt[1], 2), "-", signif(mass.cu.alt[3], 2), ")")
paste0("Current without CR/EN/VU mass: ", signif(mass.cu.r.alt[2], 2), " PgC (95%-CI: ", signif(mass.cu.r.alt[1], 2), "-", signif(mass.cu.r.alt[3], 2), ")")

# Proboscidea only
density.samples <- 10^log10dens.samples # km^-2
density.samples <- density.samples * (df$Order.1.2 == "Proboscidea")
mass.pr.cell.samples <- density.samples * mass.Pg * wet_to_carbon * cell.area # Pg Carbon
mass.pr.cell.summary <- apply(mass.pr.cell.samples, 1, function(x) c(quantile(x, 0.025), median = median(x), quantile(x, 0.975)))
mass.cu.pro <- colSums(t(mass.pr.cell.summary) * n.cells.cu)
mass.pn.pro <- colSums(t(mass.pr.cell.summary) * n.cells.pn)

paste0("Current proboscidean mass: ", signif(mass.cu.pro[2], 2), " PgC (95%-CI: ", signif(mass.cu.pro[1], 2), "-", signif(mass.cu.pro[3], 2), ")")
paste0("Present natural proboscidean mass: ", signif(mass.pn.pro[2], 2), " PgC (95%-CI: ", signif(mass.pn.pro[1], 2), "-", signif(mass.pn.pro[3], 2), ")")

paste0("Current proboscidean pct: ", signif(mass.cu.pro[2]/mass.cu[2]*100, 3), " % (95%-CI: ", signif(mass.cu.pro[1]/mass.cu[1]*100, 3), "-", signif(mass.cu.pro[3]/mass.cu[3]*100, 3), ")")
paste0("Present natural proboscidean pct: ", signif(mass.pn.pro[2]/mass.pn[2]*100, 3), " % (95%-CI: ", signif(mass.pn.pro[1]/mass.pn[1]*100, 3), "-", signif(mass.pn.pro[3]/mass.pn[3]*100, 3), ")")

# 17 086 animals 2015
#https://koedoe.co.za/index.php/koedoe/article/view/1427/2070#:~:text=A%20minimum%20of%2017%20086,management%20era%20ending%20in%201994.
kruger.loxodonta <- 17086
k.size <- 19485 #km2
kruger.density <- kruger.loxodonta/k.size
krug <- tibble(src = "---Empircal Kruger NatPark---", Binomial.1.2 = "Loxodonta_africana", dens.pr.km2 = kruger.density)

p <- which(df$Order.1.2 == "Proboscidea")
prob.pantheria <- tibble(src = "Imputed based on PanTHERIA", Binomial.1.2 = df$Binomial.1.2[p], dens.pr.km2 = round(density[p],2)) %>% arrange(dens.pr.km2)
density.alt <- 10^apply(log10dens.samples.alt, 1, median)
prob.alt <- tibble(src = "Imputed based on TetraDENS", Binomial.1.2 = df$Binomial.1.2[p], dens.pr.km2 = round(density.alt[p],2)) %>% arrange(dens.pr.km2)

pant <- read_csv("../mammal_density/builds/imputation_dataset.csv")
pant <- pant %>% filter(Order.1.2 == "Proboscidea") %>% transmute(src = "PanTHERIA", Binomial.1.2, dens.pr.km2 = 10^log10density)
tetra <- read_csv("../mammal_density/builds/imputation_dataset_PanTetra.csv")
tetra <- tetra %>% filter(Order.1.2 == "Proboscidea") %>% transmute(src = "TetraDENSITY", Binomial.1.2, dens.pr.km2 = 10^log10density)
tetra.m <- tetra %>% group_by(Binomial.1.2) %>% summarise(dens.pr.km2 = median(dens.pr.km2)) %>% mutate(src = "TetraDENSITY median")
tetra.025 <- tetra %>% group_by(Binomial.1.2) %>% summarise(dens.pr.km2 = quantile(dens.pr.km2, 0.025)) %>% mutate(src = "TetraDENSITY Quantile.025")
tetra.975 <- tetra %>% group_by(Binomial.1.2) %>% summarise(dens.pr.km2 = quantile(dens.pr.km2, 0.975)) %>% mutate(src = "TetraDENSITY Quantile.975")

tab <- bind_rows(krug, prob.pantheria, prob.alt, pant, tetra.m, tetra.025, tetra.975) %>%  arrange(dens.pr.km2) %>% print(n = 100)
write_excel_csv(tab, "output/Proboscidea_density.csv")

# The paper says:
# Current: 0.003 Pg (livestock = 0.1)
# Present natural: 0.02 Pg
# Global mammal biosmass in Carbon |||
