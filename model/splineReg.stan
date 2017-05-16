functions {
    vector build_b_spline(real[] t, real[] ext_knots, int ind, int order);
    vector build_b_spline(real[] t, real[] ext_knots, int ind, int order) {
        // INPUTS:
        //    t:          the points at which the b_spline is calculated
        //    ext_knots:  the set of extended knots
        //    ind:        the index of the b_spline
        //    order:      the order of the b-spline
        vector[size(t)] b_spline;
        vector[size(t)] w1 = rep_vector(0, size(t));
        vector[size(t)] w2 = rep_vector(0, size(t));
        if (order==1)
            for (i in 1:size(t)) // B-splines of order 1 are piece-wise constant
        b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]); 
        else {
            if (ext_knots[ind] != ext_knots[ind+order-1])
                w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) / 
                    (ext_knots[ind+order-1] - ext_knots[ind]);
            if (ext_knots[ind+1] != ext_knots[ind+order])
                w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) / 
                    (ext_knots[ind+order] - ext_knots[ind+1]);
            // Calculating the B-spline recursively as linear interpolation of 
            // two lower-order splines 
            b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) + 
                w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
        }
        return b_spline;
    }
}

data {
    int n_points;                    // number of data points
    int n_obs;                       // Number of observations
    int n_var;                       // Number of predictors
    int n_knots;                     // Number of knots
    int n_basis;                     // Number of basis functions
    matrix[n_basis, n_points] B;     // Spline basis
    vector<lower=0>[n_var] lambda;   // Smoothness penalty
    vector[n_knots]   knots;         // Sequence of knots
    vector[n_points]  Y[n_obs];      // Data matrix
    row_vector[n_var] X[n_obs];      // Design matrix
    real argvals[n_points];          // Argument values
}

transformed data {
    vector[n_var] lambda_inv;
    for (i in 1:n_var){
        lambda_inv[i] = 1 / lambda[i];
    }
}

parameters {
    row_vector[n_basis] a_raw[n_var]; 
    real<lower=0> sigma; 
    vector<lower=0>[n_var] tau;   
}

transformed parameters {
    matrix[n_var, n_points] Beta;
    row_vector[n_basis] a[n_var];
    for (i in 1:n_var){
        a[i,1] = a_raw[i,1];
        for (j in 2:n_basis){
            a[i,j] = a[i,j-1] + a_raw[i,j]*tau[i];
        }
        Beta[i] = to_row_vector(a[i]*B);
    }
}

model {
    
    // Priors
    for (i in 1:n_var){
        a_raw[i] ~ normal(0, 1);
    }
    //a0 ~ normal(0, 1);
    tau ~ normal(0, lambda_inv);
    sigma ~ normal(0, 1);
  
    //Likelihood
    for (i in 1:n_obs){
        Y[i] ~ normal(X[i] * Beta, sigma);   
    }
}