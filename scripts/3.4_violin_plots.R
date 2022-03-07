# Make violin plots
# 28/07-2021 Rasmus Ø Pedersen


# Setup data for plots ----------------------------------------------------

# a: Eco-unit consumption [MgC / yr /km2]
eco.unit.carbon.consumption <- consumption.df %>%
  left_join(eco.units, by = c("x", "y")) %>%
  mutate(period = fct_recode(period, "Present-natural" = "Present natural")) %>%
  mutate(period = fct_relevel(period, "Present-natural")) %>%
  filter(!is.na(period))

# b: Eco-unit consumption of NPP [%]
eco.unit.npp.consumption <- npp.use %>% 
  left_join(eco.units, by = c("x", "y")) %>%
  mutate(period = fct_recode(period, "Present-natural" = "Present natural")) %>%
  mutate(period = fct_relevel(period, "Present-natural")) %>%
  filter(!is.na(period))

# c: Eco-unit consumption of NPP in Last of The Wild [%]
ltw.eco.unit.npp.consumption <- eco.units %>% 
  full_join(ltw, by = c("x", "y")) %>%
  mutate(period = fct_recode(period, "Present-natural" = "Present natural")) %>%
  mutate(period = fct_relevel(period, "Present-natural")) %>%
  filter(!is.na(period)) %>% 
  bind_rows(eco.unit.npp.consumption %>%
              count(realm, biome, period) %>%
              dplyr::select(1:3))

# Setup period colors
period.colors <- c("Present-natural" = "#b2df8a", "Current" = "#a6cee3")


# Global violin plots -----------------------------------------------------

# a: Total carbon consumption [MgC / (km2 * year)]
p3a <- ggplot(eco.unit.carbon.consumption, aes(period, value, fill = period)) +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7)) +
  theme_R() +
  ylab(expression((a)~Consumption~(MgC/yr/km^2))) +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

# b: Fraction of NPP consumed [%]
p3b <- ggplot(eco.unit.npp.consumption, aes(period, value, fill = period)) +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7)) +
  theme_R() +
  ylab("(b) Fraction NPP consumed (%)") +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

# c: Fraction of NPP consumed in Last of the Wild [%]
p3c <- ggplot(ltw.eco.unit.npp.consumption, aes(period, value, fill = period)) +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7)) +
  theme_R() +
  ylab("(c) Fraction NPP consumed (LTW) (%)") +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )


p3 <- cowplot::plot_grid(p3a, p3b, p3c, nrow = 1)

if(full) {
  ggsave("./output/fig3_global_violins_full.png", p3, width = 89, height = 55, units = "mm", dpi = 600, scale = 1.5)
} else {
  ggsave("./output/fig3_global_violins200.png", p3, width = 89, height = 55, units = "mm", dpi = 600, scale = 1.5)
}



# Violin plot for ecoregions ----------------------------------------------

# a: Total carbon consumption [MgC / (km2 * year)]
p4a <- ggplot(eco.unit.carbon.consumption %>% filter(!is.na(realm)), aes(realm, value, fill = period)) +
  facet_grid(cols = vars(biome), scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7),
    show.legend = FALSE) +
  theme_bw() +
  ylab(expression((a)~Consumption~(MgC/yr/km^2))) +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# b: Fraction of NPP consumed [%]
p4b <- ggplot(eco.unit.npp.consumption %>% filter(!is.na(realm)), aes(realm, value, fill = period)) +
  facet_grid(. ~ biome, scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7),
    show.legend = FALSE) +
  theme_bw() +
  ylab("(b) Fraction NPP consumed (%)") +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# c: Fraction of NPP consumed in Last of the Wild [%]
p4c <- ggplot(ltw.eco.unit.npp.consumption %>% filter(!is.na(realm)), aes(realm, value, fill = period)) +
  facet_grid(. ~ biome, scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7),
    show.legend = FALSE) +
  theme_bw() +
  ylab("(c) Fraction NPP consumed (LTW) (%)") +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p4 <- cowplot::plot_grid(p4a, p4b, p4c, nrow = 3)

if(full) {
  ggsave("./output/fig4_eco_units_violins_full.png", p4, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.5)
} else {
  ggsave("./output/fig4_eco_units_violins200.png", p4, width = 183, height = 210, units = "mm", dpi = 600, scale = 1.5)
}
