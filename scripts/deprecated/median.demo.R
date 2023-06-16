# A simple simulation of 1 cell, 10 species, and 100 samples of consumption

set.seed(42)

# Number of species
species <- 1:10

# For one grid cell simulate 100 consumptions per species (log-distributed)
consumption <- sapply(species, function(.) rlnorm(100, meanlog = log(.), sdlog = 1))
# 10 columns (1 for each species)
# 100 rows (1 for each sample of the right skewed distributions)

# METHOD 1:
# Calculate median species consumption across simulations
species.medians <- apply(consumption, 2, median)
# Find median consumption in cell
total.consumption.1 <- sum(species.medians)
total.consumption.1

# METHOD 2:
# Calculate total species consumption per cell per simulation
cell.sums <- apply(consumption, 1, sum)
# Find median consumption across all simulations
(total.consumption.2 <- median(cell.sums))
total.consumption.2

# Change: (New method will always produce a larger number)
(total.consumption.2 - total.consumption.1)/total.consumption.1*100



# Why medians are best:
set.seed(42)
species.1 <- rlnorm(500, meanlog = 1, sdlog = 1)
species.1 %>% density %>% plot
rug(species.1)
abline(v = median(species.1), col = "blue")
abline(v = mean(species.1), col = "red")
abline(v = exp(mean(log(species.1))), col = "green", lty = 2) # Geometric mean

median(species.1)
mean(species.1)
mean(sample(species.1, replace = T))


species.1 <- rlnorm(1000, meanlog = 1, sdlog = (1:100/50))
consumption <- sapply(sd <- 1:100, function(.) rlnorm(1000, meanlog = 1, sdlog = ./50))

df <- consumption %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(),
               values_to = "value",
               names_to = c(NA, "sd"),
               names_sep = "V") %>% 
  mutate(sd = as.numeric(sd)) %>% 
  group_by(sd) %>% 
  summarise(mean = mean(value),
            median = median(value),
            sum = sum(value))

ggplot(df, aes(x = sd)) +
  geom_point(aes(y = mean, col = "mean")) +
  geom_point(aes(y = median, col = "median")) +
  geom_smooth(aes(y = mean, col = "mean")) +
  geom_smooth(aes(y = median, col = "median")) +
  geom_hline(yintercept = exp(1), lty = "dashed")





# Why medians are best:
consumption <- sapply(sd <- 1:100, function(.) rlnorm(1000, meanlog = log(1), sdlog = ./50))
log.consumption <- log(consumption)
consumption.corrected <- exp(log.consumption) %*% diag(exp(((apply(log.consumption, 2, sd))^2)/2))

mean(consumption[, 1])
median(consumption[, 1])
mean(consumption.corrected[, 1])
median(consumption.corrected[, 1])

i = 100
exp(mean(log.consumption[, i]))
mean(consumption[, i])
median(consumption[, i])
mean(consumption.corrected[, i])
median(consumption.corrected[, i])

df <- consumption %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(),
               values_to = "value",
               names_to = c(NA, "sd"),
               names_sep = "V") %>% 
  mutate(sd = as.numeric(sd)) %>% 
  group_by(sd) %>% 
  summarise(mean = mean(value),
            median = median(value),
            sum = sum(value))

ggplot(df, aes(x = sd)) +
  geom_point(aes(y = mean, col = "mean")) +
  geom_point(aes(y = median, col = "median")) +
  geom_smooth(aes(y = mean, col = "mean")) +
  geom_smooth(aes(y = median, col = "median")) +
  geom_hline(yintercept = exp(1), lty = "dashed")
