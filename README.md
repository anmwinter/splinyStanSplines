# splinyStanSplines
A work in progress implementing a fully Bayesian spline linear model in R/Stan. 
The code borrows heavily from Milad Kharratzadeh spline model at:

https://github.com/milkha/Splines_in_Stan

So far, I have a model for a univariate functional outcome and an arbitrary set of scalar/categorical fixed-effects. Eventually, I hope to accommodate separate smoothness constraints for each predictor, as well as implementing the smoothness penalty on the second derivative, which is a better penalty, theoretically speaking.
