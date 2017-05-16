construct.basis <- function(x, degree = 3, knots = NULL, n.knots = NULL){
    
    if (is.null(knots)){
        if (is.null(n.knots)){
            n.knots <- length(x)
        }
        knots <- unname(quantile(x, probs = seq(from = 0, to = 1, 
                                                length.out = n.knots)))
    }
    
    n.basis <- length(knots) + degree - 1
    B       <- t(bs(x, df = n.basis, degree = degree, intercept = TRUE))
}



