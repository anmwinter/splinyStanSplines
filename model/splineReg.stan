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
    int num_points;             // number of data points
    int num_obs;                // Number of observations
    int num_var;                // Number of predictors
    int num_knots;              // num of knots
    real<lower=0> lambda;
    vector[num_knots]  knots;   // the sequence of knots
    vector[num_points] Y[num_obs];
    row_vector[num_var]    X[num_obs];
    real idx[num_points];
}

transformed data {
    int num_basis = num_knots + 3 - 1; // total number of B-splines
    matrix[num_basis, num_points] B;     // matrix of B-splines
    vector[3 + num_knots] ext_knots_temp;
    vector[2*3 + num_knots] ext_knots; // set of extended knots
    ext_knots_temp = append_row(rep_vector(knots[1], 3), knots);
    ext_knots      = append_row(ext_knots_temp, rep_vector(knots[num_knots], 3));
    for (ind in 1:num_basis){
        B[ind,:] = to_row_vector(build_b_spline(idx, to_array_1d(ext_knots), 
            ind, 3 + 1));
    }
    B[num_knots + 3 - 1, num_points] = 1; 
}

parameters {
    row_vector[num_basis] a_raw[num_var]; 
    //vector[num_var] a0;
    real<lower=0> sigma; 
    real<lower=0> tau;   
}

transformed parameters {
    matrix[num_var, num_points] Beta;
    row_vector[num_basis] a[num_var];
    for (i in 1:num_var){
        a[i,1] = a_raw[i,1];
        for (j in 2:num_basis){
            a[i,j] = a[i,j-1] + a_raw[i,j]*tau;
        }
        Beta[i] =  to_row_vector(a[i]*B); //to_row_vector(a0[i]*to_vector(idx) +;
    }
}

model {
    // Priors
    for (i in 1:num_var){
        a_raw[i] ~ normal(0, 1);
    }
    //a0 ~ normal(0, 1);
    tau ~ normal(0, lambda);
    sigma ~ normal(0, 1);
  
    //Likelihood
    for (i in 1:num_obs){
        Y[i] ~ normal(X[i] * Beta, sigma);   
    }
}