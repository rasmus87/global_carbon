# Calculate summary statistics for consumption
# Run after 3.0_load.data.R
# 28/07-2021 Rasmus Ø Pedersen


# Calculate global consumption summaries ----------------------------------

# Summarise NPP use across all cells [%]
(npp.use.summary <- npp.use %>% 
   group_by(period) %>%
   summarise(median = median(value, na.rm= T) %>% signif(2),
             q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
             q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
             n = n()))

# Summarise NPP use (megafauna) [%]
(megafauna.npp.use.summary <- megafauna.npp.use %>%
    group_by(period) %>%
    summarise(median = median(value, na.rm= T) %>% signif(2),
              sum = sum(value, na.rm = T) %>% signif(3),
              q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
              q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
              n = n()))

# Current
# PN use is x times higher than CU use
npp.use.summary$median[2]/npp.use.summary$median[1]
# Which is x % reduction
signif(diff(npp.use.summary$median)/npp.use.summary$median[2] * 100, 2)

paste0("In the current all fauna uses ", 
       npp.use.summary$median[1], "% (95%-CI: ", npp.use.summary$q.025[1], " - ", npp.use.summary$q.975[1], ") ",
       "of NPP, and megafauna alone use ",
       megafauna.npp.use.summary$median[1], "% (95%-CI: ", megafauna.npp.use.summary$q.025[1], " - ", megafauna.npp.use.summary$q.975[1], ").")
# Present natural
paste0("In the present natural all fauna uses ", 
       npp.use.summary$median[2], " (95%-CI: ", npp.use.summary$q.025[2], " - ", npp.use.summary$q.975[2], ") ",
       "of NPP, and megafauna alone use ",
       megafauna.npp.use.summary$median[2], "% (95%-CI: ", megafauna.npp.use.summary$q.025[2], " - ", megafauna.npp.use.summary$q.975[2], ").")

# Megafauna decreased effects
paste0("An estimated reduction in NPP use by megafauna of ",
       signif((megafauna.npp.use.summary$median[2] - megafauna.npp.use.summary$median[1]) / megafauna.npp.use.summary$median[2] * 100, 2), "%.")
paste0("A reduction in NPP use from ",
       signif(megafauna.npp.use.summary$median[2], 2), "% to ", signif(megafauna.npp.use.summary$median[1], 2), "%.")

# Fraction of megafauna effect per cell:
(fraction <- mutate(npp.use, npp.consumption = value, .keep = "unused") %>% 
  left_join(mutate(megafauna.npp.use, npp.consumption.megafauna = value, .keep = "unused")) %>% 
  mutate(value = npp.consumption.megafauna/npp.consumption) %>% 
  group_by(period) %>%
  summarise(median = median(value * 100, na.rm= T) %>% signif(2),
            q.025 = quantile(value * 100, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value * 100, .975, na.rm = T) %>% signif(2),
            n = n()))


# Summarise NPP use (LTW) [%]
(ltw.use <- ltw %>% 
    group_by(period) %>% 
    filter(!is.na(value)) %>%
    summarise(median = median(value, na.rm= T) %>% signif(2),
              q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
              q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
              n = n()))
	

# Summarise carbon consumption (all) [MgC / km2 / year]
consumption.df %>% 
  group_by(period) %>%
  summarise(median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n())


# 
# 
# # Calculate total global consumption --------------------------------------
# # Units:  
# # [MgC / (km2 * year) * m2 * km2 / 10^6 m2 * 10^6 g / Mg * Pg / 10^15 g] = [PgC / year]
# tot.current <- sum(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
# tot.pres.nat <- sum(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
# diff <- tot.pres.nat - tot.current
# 
# tot.current.med <- median(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
# tot.pres.nat.med <- median(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
# tot.current.mean <- mean(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
# tot.pres.nat.mean <- mean(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
# 
# tot.current.lw <- sum(current.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T) * 10^6 / 10^15
# tot.pres.nat.lw <- sum(present.natural.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T) * 10^6 / 10^15
# diff.lw <- tot.pres.nat.lw - tot.current.lw
# 
# tot.current.hi <- sum(current.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T) * 10^6 / 10^15
# tot.pres.nat.hi <- sum(present.natural.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T) * 10^6 / 10^15
# diff.hi <- tot.pres.nat.hi - tot.current.hi
# 
# paste0("Current consumption: ", signif(tot.current, 2), " Pg Carbon / year (95%-CI: ", signif(tot.current.lw, 2), "-", signif(tot.current.hi, 2) ,")")
# paste0("Present natural consumption: ", signif(tot.pres.nat, 2), " Pg Carbon / year (95%-CI: ", signif(tot.pres.nat.lw, 2), "-", signif(tot.pres.nat.hi, 2) ,")")
# paste0("Difference: ", signif(diff, 2), " Pg Carbon / year (95%-CI: ", signif(diff.lw, 2), "-", signif(diff.hi, 2) ,")")
# 
# 
# # As fraction on NPP [%]
# 
# # Total NPP [Pg]
# tot.npp <- sum(npp[] * prod(res(npp))/10^6, na.rm = T)*10^6/10^15
# tot.npp
# # Percentage released (Doughty says 2.2-5.3)
# (tot.pres.nat - tot.current)/tot.npp * 100
# (tot.pres.nat.lw - tot.current.lw)/tot.npp * 100
# (tot.pres.nat.hi - tot.current.hi)/tot.npp * 100
# 