 
data {
  int<lower=1> M;                  // total number of patients  
  int<lower=1> K;                  // number of age groups  3
  int<lower=1> age_group[M];       // age group index for each patient  1 (children), 2 (adults),  3 (elderly)
  int<lower=1> N[M];               // number of time/VL points for each patient   
  real<lower=0> ts[M, max(N)];     // observed time points for each patient
  real y[M, max(N)];               // observed viral loads for each patient (log-scaled)
}




parameters {
  real<lower=0> mu_v_p[K];        // mean peak viral load for each age group
  real<lower=0> sigma_v_p[K];     // std dev of peak viral load for each age group
  
  real<lower=0> mu_t_p[K];        // mean time to peak viral load for each age group
  real<lower=0> sigma_t_p[K];     // std dev of time to peak viral load for each age group
  
  real<lower=0> mu_lambda_g[K];        // mean growth rate for each age group
  real<lower=0> sigma_lambda_g[K];     // std dev of growth rate for each age group
  
  real<lower=0> mu_lambda_d[K];        // mean decline rate for each age group
  real<lower=0> sigma_lambda_d[K];     // std dev of decline rate for each age grou
  
  real<lower=0> mu_incub[K]; // mean incubation periods for each age group 
  real<lower=0> sigma_incub[K]; // std dev of incubation periods for each age group 
  
 // real mu_V0[K]; // mean V0 for each age group 
//  real<lower=0> sigma_V0[K]; // std dev of V0 for each age group 
  
  real<lower=0> v_p[M];           // peak viral load for each patient
  real<lower=0> t_p[M];          // time to peak viral load for each patient
  real<lower=0> lambda_g[M];     // growth rate for each patient
  real<lower=0> lambda_d[M];     // decline rate for each patient
  real<lower=0> incub[M];       // incubation periods for each patient
 // real V0[M];          // V0 for each patient
  
   
  real<lower=0> sigma;             // standard deviation of the observation noise
  
  
} 
  
 model {
  real y_hat[M, max(N)];
  
  // Priors
  for (k in 1:K) {
    mu_v_p[k] ~ normal(0,3); 
    mu_t_p[k] ~ normal(0,3);  
    mu_lambda_g[k] ~ normal(1,3); 
    mu_lambda_d[k] ~ normal(1,3);  
    mu_incub[k] ~ normal(0,3);
    
    

    sigma_v_p[k] ~ normal(0,2); //cauchy(0, 1.5); // weakly informative prior
    sigma_t_p[k] ~ normal(0,2); // cauchy(0, 1.5); // weakly informative prior
    sigma_lambda_g[k] ~ normal(0,2); //cauchy(0, 1.5); // weakly informative prior
    sigma_lambda_d[k] ~ normal(0,2); //cauchy(0, 1.5); // weakly informative prior
    sigma_incub[k] ~ normal(0,2); // normal(1.24, 0.1); 
 
    
 
  
  }
  sigma ~ cauchy(0, 1); // weakly informative prior
  
  
  for (i in 1:M) {
    v_p[i] ~ normal(mu_v_p[age_group[i]], sigma_v_p[age_group[i]]);
    t_p[i] ~ normal(mu_t_p[age_group[i]], sigma_t_p[age_group[i]]);
    lambda_g[i] ~ normal(mu_lambda_g[age_group[i]], sigma_lambda_g[age_group[i]]);
    lambda_d[i] ~ normal(mu_lambda_d[age_group[i]], sigma_lambda_d[age_group[i]]);
    incub[i] ~ normal(mu_incub[age_group[i]], sigma_incub[age_group[i]]);
    //V0[i] ~ normal(mu_V0[age_group[i]], sigma_V0[age_group[i]]);
  }
  
  for (i in 1:M) {
     if (age_group[i] != 2){
       for (j in 1:N[i]) {
      y[i, j] ~ normal(log(2*10^v_p[i]/( exp(-lambda_g[i]*((ts[i, j]+incub[i])-t_p[i])) +  exp(lambda_d[i]*((ts[i, j]+incub[i])-t_p[i])))), sigma); 
        }
       }
    else{
    for (j in 1:N[i]) {
      y[i, j] ~ normal(log(2*10^v_p[i]/( exp(-lambda_g[i]*(ts[i, j]-t_p[i])) +  exp(lambda_d[i]*(ts[i, j]-t_p[i])))), sigma); 
         }
       }
   }
   
   
 }
  
  
  
