library(rstan)
library(ggplot2)
library(ggthemes)

# Simulate data
source('simData.R')

# Plot true effects
plot.frame <- data.frame(Beta = as.factor(rep(1:4, each = num.points)),
                         Index  = rep(idx, 4),
                         Y      = as.numeric(t(B)))
png(file = 'plots/trueFactors.png',
    width = 430, height = 340, res = 100)
ggplot(plot.frame, aes(x = Index, y = Y, group = Beta, color = Beta)) +
    geom_line(size = 2) +
    scale_colour_tableau() +
    theme_minimal()
dev.off()

# Data for stan
stan.data <- list(num_points = num.points,
                  num_obs    = num.obs,
                  num_var    = 4,
                  num_knots  = 5,
                  lambda     = 1,
                  knots      = seq(0, 1, length.out = 5),
                  Y          = Y,
                  X          = X,
                  idx        = idx)

# Fit model
model <- stan_model('model/splineReg.stan')
fit   <- sampling(object = model, data = stan.data,
                  iter = 1000, chains = 3, cores = 2)
samples <- extract(fit)

# Plot estimate
plot.frame <- data.frame(Estimate = as.factor(rep(1:4, each = num.points)),
                         Index  = rep(idx, 4),
                         Y      = as.numeric(t(colMeans(samples$Beta))))
png(file = 'plots/estimatedFactors.png',
    width = 430, height = 340, res = 100)
ggplot(plot.frame, aes(x = Index, y = Y, group = Estimate, color = Estimate)) +
    geom_line(size = 2) +
    scale_colour_tableau() +
    theme_minimal()
dev.off()
