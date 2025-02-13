#' Simulation, which compares CI methods, when M needs to be estimated using
#' ROT methods and the variance is heteroscedastic.
sim_hetero_error_variance <- function(){
  
  #-------------------         fix parameters          ---------------------------
  
  S <- 10000
  alpha <- 0.05
  
  #-----        Analysis of coverage probability and interval length          ----
  
  n <- c(500)
  
  # Basis for construction of combinations
  data_model <- c(
    "design_1",
    "design_2",
    "design_3",
    "design_4",
    "design_5"
  )
  
  
  mrot_method <- c("true_m","Imbens_scale_4","spline_p_2_k_4")
  M <- c(8)
  se_method_J <- c(3,6)
    
  noise_method <- c(
    "homoscedastic",
    "center",
    "border",
    "linear",
    "quadratic"
  )
  
  kernel <- c("triangular")
  
  # Methods with uniform bandwidth
  uniform_params <- tibble::tribble(
    ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
    "honest",      "MSE",               TRUE,      "nn"
  ) 
  
  uniform_grid <- expand.grid(
    n           = n,
    data_model  = data_model,
    mrot_method = mrot_method,
    M           = M,
    noise_method = noise_method,
    kernel      = kernel,
    se_method_J = se_method_J 
  )
  
  
  uniform_params_expanded <- dplyr::cross_join(uniform_params, uniform_grid)
  
  # Methods with no uniform bandwidth
  not_uniform_params <- tibble::tribble(
    ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
    "rbc",      "MSE",               FALSE,      "nn"
  )
  
  not_uniform_grid <- expand.grid(
    n           = n,
    data_model  = data_model,
    mrot_method = NA,
    M           = M,
    noise_method = noise_method,
    kernel      = kernel,
    se_method_J = se_method_J
  )
  
  not_uniform_params_expanded <- dplyr::cross_join(not_uniform_params, not_uniform_grid)
  
  # Combine Grids
  
  coverage_prob_grid <- rbind(not_uniform_params_expanded, uniform_params_expanded)
  
  grid_length <- nrow(coverage_prob_grid)
  
  # Estimate coverage probability for all combinations
  for (i in c(1:grid_length)){
    
    param <- coverage_prob_grid[i,]
    
    estimates <- simulation( 
      S                 = S,
      data_model        = as.character(param$data_model),
      n                 = as.integer(param$n),
      mrot_method       = as.character(param$mrot_method),
      M                 = as.integer(param$M),
      noise_method      = as.character(param$noise_method),
      kernel            = as.character(param$kernel),
      ci_method         = as.character(param$ci_method),
      bw_method         = as.character(param$bw_method),
      bw_method_uniform = as.logical(param$bw_method_uniform),
      se_method         = as.character(param$se_method),
      se_method_J       = as.integer(param$se_method_J),
      alpha             = alpha
    )
    
    coverage_prob_grid[i,"coverage_prob"]   <- estimates$coverage_prob
    coverage_prob_grid[i,"interval_length"] <- estimates$interval_length
    coverage_prob_grid[i,"tau_hat_se"]      <- estimates$tau_hat_se
    coverage_prob_grid[i,"tau_hat"]         <- estimates$tau_hat
    coverage_prob_grid[i,"h_hat"]           <- estimates$h_hat
    coverage_prob_grid[i,"b_hat"]           <- estimates$b_hat 
    coverage_prob_grid[i,"m_hat"]           <- estimates$m_hat 
    coverage_prob_grid[i,"m"]               <- estimates$M
    coverage_prob_grid[i,"m_sd"]            <- estimates$m_sd
    
    print(paste0(i," / ", grid_length))
    print(coverage_prob_grid[i,])
  }
  
  sim_hetero_error_variance <- coverage_prob_grid
  save_path <- file.path(getwd(),"data","simulation_results","sim_hetero_error_variance.rds")
  saveRDS(sim_hetero_error_variance, save_path)
}  