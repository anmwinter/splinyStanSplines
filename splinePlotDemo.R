library(rstan)
library(ggplot2)
library(ggthemes)
source('R/constructBasis.R')
source('R/splineHeatPlot.R')

# Simulate data
source('R/simData2.R')

# Plot true effects
plot.frame <- data.frame(Beta  = as.factor(rep(1:2, each = num.points)),
                         Index = rep(idx, 2),
                         Y     = as.numeric(t(B)))
png(file = 'plots/trueFactorsSplinePlot.png',
    width = 430, height = 340, res = 100)
ggplot(plot.frame, aes(x = Index, y = Y, group = Beta, color = Beta)) +
    geom_line(size = 2) +
    scale_colour_tableau() +
    theme_minimal()
dev.off()

# Data for stan
B <- construct.basis(idx, degree = 3, knots = NULL, n.knots = 10)
stan.data <- list(n_points = num.points,
                  n_obs    = num.obs,
                  n_var    = 2,
                  n_knots  = 10,
                  n_basis  = dim(B)[1],
                  lambda   = c(1,1),
                  knots    = seq(0, 1, length.out = 10),
                  Y        = Y,
                  X        = X,
                  argvals  = idx,
                  B        = B)

# Fit model
model <- stan_model('model/splineReg.stan')
fit   <- sampling(object = model, data = stan.data,
                  iter = 1000, chains = 3, cores = 2,
                  control = list(adapt_delta = 0.99))
samples <- extract(fit)

# Plot effect estimate
png(file = 'plots/heatmapSplinePlot.png',
    width = 430, height = 340, res = 100)
spline.heat.plot(samples$a[,2,], B, idx, 
                 xlab = 'Index', ylab = 'Effect')
dev.off()
