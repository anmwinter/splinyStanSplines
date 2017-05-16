# Simulate data
num.obs    <- 100
num.points <- 25
sigma      <- .2
idx        <- seq(0, 1, length.out = num.points)
X <- matrix(c(rep(1, num.obs),
              rep(c(0,1), each = num.obs / 2)), 
             ncol = 2, byrow = F)
B <- matrix(c(0  * idx,
             sqrt(idx)), 
            nrow = 2, byrow = T)

E <- lapply(1:num.points, function(x) rnorm(num.obs, sd = x*sigma + .01))
E <- do.call(cbind, E)
Y <- X %*% B + E

