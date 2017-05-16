# Simulate data
num.obs    <- 100
num.points <- 25
sigma      <- .05
idx        <- seq(0, 1, length.out = num.points)
X <- matrix(c(rep(1, num.obs),
              rep(c(0,1), each = num.obs / 2),
              rep(c(0,1), each = num.obs / 4, 2),
              rnorm(num.obs)), 
             ncol = 4, byrow = F)
B <- matrix(c(0  * idx,
             .5  * idx,
             .1 * idx^2,
             .05 * sin(8*idx)), 
            nrow = 4, byrow = T)
Y <- X %*% B + matrix(rnorm(num.obs*num.points, sd = sigma),
                      ncol = num.points, nrow = num.obs)
