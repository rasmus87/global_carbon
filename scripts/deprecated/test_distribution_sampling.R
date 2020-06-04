# Set-up
set.seed(1)
n = 100
log.x0 <- rlnorm(n)
x0 <- exp(log.x0)

# Find a function for x1 and x2
log.x1 <- 10 + log.x0 + rnorm(n)/2
m1 <- lm(log.x1~log.x0)
log.x2 <- 10 + -1/2 * log.x0 + rnorm(n)/2
m2 <- lm(log.x1~log.x0)

# Data on x0 i want to sample:
log.X <- rlnorm(100, sdlog = .1)
X <- exp(log.X)

# Estimates of x1 and x2 at X
x1.est <- predict(m1, list(log.x0 = log.X))
x2.est <- predict(m1, list(log.x0 = log.X))

# Estimate of y at X
y <- exp(x1.est) * exp(x2.est)

# Estimate of sum in different groups:
group <- cut(X, seq(2,4,.25))
aggregate(y, by=list(group), sum)



# Make mean prediction including se
x1.pred <- predict(m1, list(log.x0 = log.X), se.fit = T)
x2.pred <- predict(m2, list(log.x0 = log.X), se.fit = T)

log.X1.est <- sapply(1:length(X),
                           FUN = function(i) {
                             rnorm(n = n,
                                   mean = x1.pred$fit[i],
                                   sd = x1.pred$se.fit[i])
                           }
                          )

log.X2.est <- sapply(1:length(X),
                     FUN = function(i) {
                       rnorm(n = n,
                             mean = x2.pred$fit[i],
                             sd = x2.pred$se.fit[i])
                     }
)

y.distribution <- exp(log.X1.est) * exp(log.X2.est)

y.mean <- apply(y.distribution, 1, mean)
y.med <- apply(y.distribution, 1, median)
y.q05 <- apply(y.distribution, 1, quantile, probs = 0.05)
y.q95 <- apply(y.distribution, 1, quantile, probs = 0.95)

agg <- aggregate(t(y.distribution), by=list(group), sum)
apply(agg[-1], 1, mean)
apply(agg[-1], 1, median)
apply(agg[-1], 1, quantile, probs = 0.05)
apply(agg[-1], 1, quantile, probs = 0.95)


y.groups <- aggregate(y, by=list(group), sum)
barplot(y.groups$x)

barplot(apply(agg[-1], 1, median))

group <- droplevels(group)
df2 <- data.frame(group = rep(levels(group), 3), 
           method = gl(3, nlevels(group), labels = c("y.est", "sample mean", "sample median")), 
           y.sum = c(aggregate(y, by=list(group), sum)$x,
                     apply(agg[-1], 1, mean),
                     apply(agg[-1], 1, median)),
           y.lo = c(rep(NA, nlevels(group)),
                     rep(apply(agg[-1], 1, quantile, probs = 0.05), 2)),
           y.hi = c(rep(NA, nlevels(group)),
                     rep(apply(agg[-1], 1, quantile, probs = 0.95), 2)))

ggplot(df2, aes(x = group, y = y.sum, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = y.lo, ymax = y.hi), position = position_dodge(.8), width = .5)
