library(rstan)

set.seed(1234995)
data_stan <- readRDS("data_stan_53_patients.rds")
K <- data_stan$K
M <- data_stan$M

init_condition1 <- list(
    mu_v_p = runif(K, 2, 8),        # Random initial values between 1 and 10
    sigma_v_p = runif(K, 0, .1),    # Random initial values between 0.1 and 1
    mu_t_p = runif(K, 2,6),        # Random initial values between 1 and 10
    sigma_t_p = runif(K, 0, 0.1),    # Random initial values between 0.1 and 1
    mu_lambda_g = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_g = runif(K, 0,0.1),  # Random initial values between 0.01 and 0.1
    mu_lambda_d = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_d = runif(K, 0, 0.1),  # Random initial values between 0.01 and 0.1
    mu_incub = runif(K,1,7),
    sigma_incub = runif(K,3,7),
    #mu_V0 = runif(K, -3,3),
    #sigma_V0 = runif(K, 0, 1),
    v_p = runif(M, 2,8),           # Random initial values between 1 and 10
    t_p = runif(M, 2,6),           # Random initial values between 1 and 10
    lambda_g = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    lambda_d = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    incub = runif(M, 3,7),
    #V0 = runif(M,-3,3),
    sigma = 1                        # Initial value for observation noise
  )

init_condition2 <- list(
    mu_v_p = runif(K, 2, 8),        # Random initial values between 1 and 10
    sigma_v_p = runif(K, 0, .1),    # Random initial values between 0.1 and 1
    mu_t_p = runif(K, 2,6),        # Random initial values between 1 and 10
    sigma_t_p = runif(K, 0, 0.1),    # Random initial values between 0.1 and 1
    mu_lambda_g = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_g = runif(K, 0,0.1),  # Random initial values between 0.01 and 0.1
    mu_lambda_d = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_d = runif(K, 0, 0.1),  # Random initial values between 0.01 and 0.1
    mu_incub = runif(K,1,7),
    sigma_incub = runif(K,3,7),
    #mu_V0 = runif(K, -3,3),
    #sigma_V0 = runif(K, 0, 1),
    v_p = runif(M, 2,8),           # Random initial values between 1 and 10
    t_p = runif(M, 2,6),           # Random initial values between 1 and 10
    lambda_g = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    lambda_d = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    incub = runif(M, 3,7),
    #V0 = runif(M,-3,3),
    sigma = 1                        # Initial value for observation noise
  )

init_condition3 <- list(
    mu_v_p = runif(K, 2, 8),        # Random initial values between 1 and 10
    sigma_v_p = runif(K, 0, .1),    # Random initial values between 0.1 and 1
    mu_t_p = runif(K, 2,6),        # Random initial values between 1 and 10
    sigma_t_p = runif(K, 0, 0.1),    # Random initial values between 0.1 and 1
    mu_lambda_g = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_g = runif(K, 0,0.1),  # Random initial values between 0.01 and 0.1
    mu_lambda_d = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_d = runif(K, 0, 0.1),  # Random initial values between 0.01 and 0.1
    mu_incub = runif(K,1,7),
    sigma_incub = runif(K,3,7),
    #mu_V0 = runif(K, -3,3),
    #sigma_V0 = runif(K, 0, 1),
    v_p = runif(M, 2,8),           # Random initial values between 1 and 10
    t_p = runif(M, 2,6),           # Random initial values between 1 and 10
    lambda_g = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    lambda_d = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    incub = runif(M, 3,7),
    #V0 = runif(M,-3,3),
    sigma = 1                        # Initial value for observation noise
  )

init_condition4 <- list(
    mu_v_p = runif(K, 2, 8),        # Random initial values between 1 and 10
    sigma_v_p = runif(K, 0, .1),    # Random initial values between 0.1 and 1
    mu_t_p = runif(K, 2,6),        # Random initial values between 1 and 10
    sigma_t_p = runif(K, 0, 0.1),    # Random initial values between 0.1 and 1
    mu_lambda_g = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_g = runif(K, 0,0.1),  # Random initial values between 0.01 and 0.1
    mu_lambda_d = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_d = runif(K, 0, 0.1),  # Random initial values between 0.01 and 0.1
    mu_incub = runif(K,1,7),
    sigma_incub = runif(K,3,7),
    #mu_V0 = runif(K, -3,3),
    #sigma_V0 = runif(K, 0, 1),
    v_p = runif(M, 2,8),           # Random initial values between 1 and 10
    t_p = runif(M, 2,6),           # Random initial values between 1 and 10
    lambda_g = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    lambda_d = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    incub = runif(M, 3,7),
    #V0 = runif(M,-3,3),
    sigma = 1                        # Initial value for observation noise
  )

init_condition5 <- list(
    mu_v_p = runif(K, 2, 8),        # Random initial values between 1 and 10
    sigma_v_p = runif(K, 0, .1),    # Random initial values between 0.1 and 1
    mu_t_p = runif(K, 2,6),        # Random initial values between 1 and 10
    sigma_t_p = runif(K, 0, 0.1),    # Random initial values between 0.1 and 1
    mu_lambda_g = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_g = runif(K, 0,0.1),  # Random initial values between 0.01 and 0.1
    mu_lambda_d = runif(K, 1,4),  # Random initial values between 0.1 and 1
    sigma_lambda_d = runif(K, 0, 0.1),  # Random initial values between 0.01 and 0.1
    mu_incub = runif(K,1,7),
    sigma_incub = runif(K,3,7),
    #mu_V0 = runif(K, -3,3),
    #sigma_V0 = runif(K, 0, 1),
    v_p = runif(M, 2,8),           # Random initial values between 1 and 10
    t_p = runif(M, 2,6),           # Random initial values between 1 and 10
    lambda_g = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    lambda_d = runif(M, 1, 4),     # Random initial values between 0.1 and 1
    incub = runif(M, 3,7),
    #V0 = runif(M,-3,3),
    sigma = 1                        # Initial value for observation noise
  )



options(mc.cores=parallel::detectCores()) # to utilise all cores available in your computer

fit_rsv_model <- stan("RSV_model_incubation2.stan",
                     data = data_stan,
                     seed = 20240616,  # set random seed for reproducibility
                     pars = c("mu_v_p","mu_t_p", "mu_lambda_g", "mu_lambda_d","mu_incub",
                              "sigma_v_p", "sigma_t_p", "sigma_lambda_g", "sigma_lambda_d", "sigma_incub", 
                              "v_p", "t_p", "lambda_g", "lambda_d", "incub",  "sigma"),
                     iter = 5000,
                     chains = 5,
                     init = list(init_condition1, 
                                 init_condition2,
                                 init_condition3,
                                 init_condition4,
                                 init_condition5),
                              
                     warmup = 2500,
                     control = list(adapt_delta = 0.99, max_treedepth = 15))


saveRDS(fit_rsv_model, "fit_rsv_results_incubation_53_patients_5chains.rds")

