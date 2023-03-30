# Test means

# Estimate consumption per species

# Load libraries
library(tidyverse)



# Load data ---------------------------------------------------------------

# Load traits data
df <- read_csv("builds/data.csv", col_types = cols())

# Load population density posterior distribution
dens <- read_csv("../mammal_density/builds/densities_post.pred.csv") # log10 individuals / km2
# Make sure all species are there
all(df$Binomial.1.2 %in% names(dens))
# Subset
dens <- dens[df$Binomial.1.2]
# Make sure alignment is right
stopifnot(all.equal(names(dens), df$Binomial.1.2))

dens.data <- read_csv("../mammal_density/builds/imputation_dataset_PanTHERIA.csv") # log10 individuals / km2

sp <- "Loxodonta_africana"
sp <- "Canis_lupus"
sp <- "Vulpes_vulpes"
sp <- "Sus_scrofa"
i <- which(df$Binomial.1.2 == sp)
j <- which(dens.data$Binomial.1.2 == sp)


library(rethinking)
x <- HPDI(dens[, i], prob = .95) %>% as_tibble()

dens.s <- dens %>% select(i) %>% rename_all(function(.) "value")

bayestestR::map_estimate(10^dens.s$value)

ggplot(dens.s, aes(value)) +
  # geom_rect(aes(xmin = x$value[1], xmax = x$value[2], ymin = 0, ymax = 1), fill = "yellow") +
  geom_density() +
  geom_vline(aes(xintercept = df$log10density[i], col = "official"), lwd = 2) +
  geom_vline(aes(xintercept = mean(value), col = "mean")) +
  geom_vline(aes(xintercept = median(value), col = "median")) +
  geom_vline(aes(xintercept = bayestestR::map_estimate(value), col = "MAP")) +
  geom_vline(aes(xintercept = dens.data$log10density[j], col = "empirical"), lty = 2)

y <- 10^x
z <- HPDI(10^dens.s$value, prob = .95) %>% as_tibble()
p <- 10^quantile(dens.s$value, probs = c(.025, .975)) %>% as_tibble()

exp((log(10) * sd(dens.s$value))^2/2) * 10^mean(dens.s$value)
test <- log(10^dens.s$value)
exp(mean(test) + sd(test)^2/2)


sp <- "Loxodonta_africana"
i <- which(df$Binomial.1.2 == sp)
j <- which(dens.data$Binomial.1.2 == sp)
dens.s <- dens %>% select(i) %>% rename_all(function(.) "value")
x <- log(10^dens.s$value)

mu <- mean(x)
sd2 <- sd(x)^2

# Median
exp(mu)
median(exp(x))
# Mean
exp(mu + sd2/2)
mean(exp(x))
# Geometric mean
exp(mean(x))

y <- exp(x) * exp(x)

mean(y)
exp(mean(log(y)))
median(y)
exp(mean(x)) * exp(mean(x))


ggplot(dens.s, aes(10^value)) +
  # geom_rect(aes(xmin = y$value[1], xmax = y$value[2], ymin = 0, ymax = 1), fill = "yellow", alpha = .5) +
  # geom_rect(aes(xmin = z$value[1], xmax = z$value[2], ymin = 0, ymax = .9), fill = "blue", alpha = .5) +
  # geom_rect(aes(xmin = p$value[1], xmax = p$value[2], ymin = 0, ymax = .8), fill = "green", alpha = .5) +
  geom_density(bw = "SJ") +
  geom_vline(aes(xintercept = 10^df$log10density[i], col = "official"), lwd = 2) +
  # geom_vline(aes(xintercept = mean(10^dens[, i]), col = "mean")) +
  geom_vline(aes(xintercept = median(10^value), col = "median")) +
  geom_vline(aes(xintercept =exp((log(10) * sd(value))^2/2) * 10^median(value), col = "corrected mean")) +
  geom_vline(aes(xintercept = 10^bayestestR::map_estimate(value), col = "MAP")) +
  geom_vline(aes(xintercept = bayestestR::map_estimate(10^value), col = "MAP2")) +
  geom_vline(aes(xintercept = 10^dens.data$log10density[j], col = "empirical"), lty = 2)


ggplot(dens.s, aes(10^value)) +
  # geom_rect(aes(xmin = y$value[1], xmax = y$value[2], ymin = 0, ymax = 1), fill = "yellow", alpha = .5) +
  # geom_rect(aes(xmin = z$value[1], xmax = z$value[2], ymin = 0, ymax = .9), fill = "blue", alpha = .5) +
  # geom_rect(aes(xmin = p$value[1], xmax = p$value[2], ymin = 0, ymax = .8), fill = "green", alpha = .5) +
  geom_histogram(binwidth = .6)


df.all <- left_join(dens.data, df, by = "Binomial.1.2")

ggplot(df.all, aes(log10(Mass.g), log10density.x, col = "measured")) +
  geom_point() +
  geom_point(aes(y = log10density.y, col = "predicted")) +
  geom_segment(aes(x = log10(Mass.g), xend = log10(Mass.g), y = log10density.x, yend = log10density.y),
               arrow = arrow(length = unit(0.01, "npc")))
                 

