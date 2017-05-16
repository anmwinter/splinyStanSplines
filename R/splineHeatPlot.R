spline.heat.plot <- function(coefs, B, idx, xlab = '', ylab = '', alpha = .025){
    
    n.samples <- dim(coefs)[1]
    n.basis   <- dim(coefs)[2]
    
    # Data frame for plotting
    l <- do.call(rbind, lapply(1:n.samples, function(i) coefs[i,] %*% B))
    plot.frame <- data.frame(Sample = as.factor(rep(1:n.samples, 
                                                    each = length(idx))),
                             X = idx,
                             Y = as.numeric(t(l)))
    
    
    # Create plot
    g <- ggplot(plot.frame, aes(x = X, y = Y)) +
            scale_colour_tableau() +
            theme_minimal() +
            xlab(xlab) +
            ylab(ylab) +
            geom_line(alpha=alpha, color = 'orange', aes(group = Sample)) +
            stat_density_2d(geom = "raster", aes(fill  = ..density..,
                                                 alpha = ..density..), 
                            contour = FALSE) +
            scale_fill_gradient(low = "white", high = 'red') +
            theme(legend.position = "none")
        
    return(g)
}