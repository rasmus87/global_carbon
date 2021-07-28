SF_fig4a <- ggplot(eco.unit.carbon.consumption %>% filter(!is.na(realm)), aes(realm, value, fill = period)) +
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
  ylab(expression(Consumption~(MgC/yr/km^2))) +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("./output/SF_fig4a.png", SF_fig4a, width = 183, height = 85, units = "mm", dpi = 600, scale = 1.5)
