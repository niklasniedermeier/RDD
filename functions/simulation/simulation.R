#' Run a Single Simulation Step for RDD
#'
#' Executes one step of an RDD simulation using specified data, estimation methods, and parameters.
#'
#' @param data_model `character` Data generating process model
#' @param n `numeric` Sample size
#' @param kernel `character` Kernel type for RDD estimation
#' @param mrot_method `character` Method for estimating Hölder constant
#' @param M `numeric` True Hölder constant
#' @param noise_method `character` Method for generating noise
#' @param ci_method `character` Confidence interval method
#' @param bw_method `character` Bandwidth selection method
#' @param bw_method_uniform `logical` Use uniform bandwidth
#' @param se_method `character` Standard error estimation method
#' @param se_method_J `integer` Nearest neighbors for SE estimation
#' @param alpha `numeric` Significance level
#'
#' @return `list` Coverage estimates.
#'
simulation_step <- function(
  data_model,
  n,
  kernel,
  mrot_method,
  M,
  noise_method,
  ci_method,
  bw_method,
  bw_method_uniform,
  se_method,
  se_method_J,
  alpha
){
  
  #-----------------------------------------------------------------------------
  # Select data generating process
  data <- dgp(data_model = data_model, n = n, M = M, noise_method = noise_method)
  
  X <- data$X
  Y <- data$Y
  cutoff <- data$cutoff
  tau <- data$tau
 
  #-----------------------------------------------------------------------------
  # Get Hölder constant estimate
  if (bw_method_uniform){
    
    if (mrot_method == "true_m"){
      m_hat <- M
    }
    

    if (grepl("^M_[0-9]+(\\.[0-9]+)?$", mrot_method)) {
      # Specify constant estimators. For example "M_4" results in the constant
      # estimator m_hat = 4
      m_hat <- as.numeric(sub("^M_([0-9]+(?:\\.[0-9]+)?)$", "\\1", mrot_method))
    }
    
    if (mrot_method == "Kolesar_hb_15"){
      m_hat <- mrot_lower_bound(X = X, Y = Y, cutoff = cutoff, alpha = alpha, s = 15, estimate = "hb")
    }
    
    if (mrot_method == "Kolesar_hb_20"){
      m_hat <- mrot_lower_bound(X = X, Y = Y, cutoff = cutoff, alpha = alpha, s = 20, estimate = "hb")
    }
    
    if (mrot_method == "Kolesar_hb_25"){
      m_hat <- mrot_lower_bound(X = X, Y = Y, cutoff = cutoff, alpha = alpha, s = 25, estimate = "hb")
    }
    
    if (mrot_method == "Kolesar_hb_30"){
      m_hat <- mrot_lower_bound(X = X, Y = Y, cutoff = cutoff, alpha = alpha, s = 30, estimate = "hb")
    }
    
    if (mrot_method == "poly_p_2"){
      m_hat <- mrot(X = X, Y = Y, method = "poly", c = cutoff, p = 2)
    }
    
    if (mrot_method == "poly_p_3"){
      m_hat <- mrot(X = X, Y = Y, method = "poly", c = cutoff, p = 3)
    }
    
    if (mrot_method == "poly_p_4"){
      m_hat <- mrot(X = X, Y = Y, method = "poly", c = cutoff, p = 4)
    }
    
    if (mrot_method == "poly_p_2_se"){
      m_hat <- mrot(X = X, Y = Y, method = "poly_2_se", c = cutoff, p = 2)
    }
    
    if (mrot_method == "Imbens_scale_2"){
      m_hat <- 2 * mrot(X = X, Y = Y, method = "poly", c = cutoff, p = 2)
    }
    
    if (mrot_method == "Imbens_scale_3"){
      m_hat <- 3 * mrot(X = X, Y = Y, method = "poly", c = cutoff, p = 2)
    }
    
    if (mrot_method == "Imbens_scale_4"){
      m_hat <- 4 * mrot(X = X, Y = Y, method = "poly", c = cutoff, p = 2)
    }
    
    if (mrot_method == "spline_p_2_k_2"){
      m_hat <- mrot(X = X, Y = Y, method = "spline", c = cutoff, p = 2, nknots = 2)
    }
    
    if (mrot_method == "spline_p_2_k_3"){
      m_hat <- mrot(X = X, Y = Y, method = "spline", c = cutoff, p = 2, nknots = 3)
    }
    
    if (mrot_method == "spline_p_2_k_4"){
      m_hat <- mrot(X = X, Y = Y, method = "spline", c = cutoff, p = 2, nknots = 4)
    }
    
  }else{
    #Use dummy m_hat for rd()
    m_hat <- NA
  }
  
  #-----------------------------------------------------------------------------
  # Get RD estimates
  rd_estimates <- suppressMessages(
    rd(
      Y = Y,
      X = X,
      cutoff = cutoff,
      M = m_hat,
      kernel = kernel,
      ci_method = ci_method,
      bw_method = bw_method,
      bw_method_uniform = bw_method_uniform,
      se_method = se_method,
      se_method_J = se_method_J,
      alpha = alpha
    )
  )
  
  #-----------------------------------------------------------------------------
  # evaluate coverage of simulation step
  conf.low       <- rd_estimates$conf.low
  conf.high      <- rd_estimates$conf.high
  check_coverage <- (conf.low <= tau) & (tau <= conf.high)  
  
  coverage_estimates <- list(
    check_coverage    = check_coverage,
    conf.low          = conf.low,
    conf.high         = conf.high,
    tau_hat           = rd_estimates$tau_hat,
    tau_hat_se        = rd_estimates$tau_hat_se,
    cv                = rd_estimates$cv,
    h_hat             = rd_estimates$h_hat,
    b_hat             = rd_estimates$b_hat,
    m_hat             = m_hat
  )
  
  return(coverage_estimates)
}


#' Run Multiple Simulations for RDD
#'
#' Runs `S` RDD simulations and aggregates results.
#'
#' @param S `numeric` Number of simulation steps
#' @param data_model `character` Data generating process model
#' @param n `numeric` Sample size
#' @param mrot_method `character` Method for estimating Hölder constant
#' @param M `numeric` True Hölder constant
#' @param noise_method `character` Method for generating noise
#' @param kernel `character` Kernel type for RDD estimation
#' @param ci_method `character` Confidence interval method
#' @param bw_method `character` Bandwidth selection method
#' @param bw_method_uniform `logical` Use uniform bandwidth
#' @param se_method `character` Standard error estimation method
#' @param se_method_J `integer` Nearest neighbors for SE estimation
#' @param alpha `numeric` Significance level
#'
#' @return `list` Aggregated simulation results.
#'
simulation <- function(
  S,
  data_model,
  n,
  mrot_method,
  M,
  noise_method,
  kernel,
  ci_method,
  bw_method,
  bw_method_uniform,
  se_method,
  se_method_J,
  alpha
){
  
  simulations <- lapply((1:S), 
    function(s){
      
      coverage_estimates <- simulation_step(
        data_model        = data_model,
        n                 = n,
        kernel            = kernel,
        mrot_method       = mrot_method,
        M                 = M,
        noise_method      = noise_method, 
        ci_method         = ci_method,
        bw_method         = bw_method,
        bw_method_uniform = bw_method_uniform,
        se_method         = se_method,
        se_method_J       = se_method_J, 
        alpha             = alpha
      )
      return(coverage_estimates)
    }
  )
  
  simulations <- bind_rows(simulations)
 
  conf.low        <- mean(simulations$conf.low)
  conf.high       <- mean(simulations$conf.high)
  interval_length <- abs(conf.high - conf.low)
  
  params_and_estimates <-
    list(
      coverage_prob     = mean(simulations$check_coverage),
      interval_length   = interval_length,
      tau_hat_se        = mean(simulations$tau_hat_se),
      tau_hat           = mean(simulations$tau_hat),
      h_hat             = mean(simulations$h_hat),
      b_hat             = mean(simulations$b_hat),
      cv                = mean(simulations$cv),
      m_hat             = mean(simulations$m_hat),
      m_sd              = sd(simulations$m_hat),
      M                 = M
    )
  
  return(params_and_estimates)
}

