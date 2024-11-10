coverage_prob_step <- function(
  data_model,
  n,
  kernel,
  mrot,
  ci_method,
  bw_method,
  bw_method_uniform,
  se_method,
  alpha
){
  
  #-----------------------------------------------------------------------------
  # Select data generating process
  data <- dgp(data_model = data_model, n = n)
  
  X <- data$X
  Y <- data$Y
  M <- data$M
  cutoff <- data$cutoff
  tau <- data$tau
  
  #-----------------------------------------------------------------------------
  # Get Hölder constant estimate
  if (bw_method_uniform){
    
    if (mrot == "poly_2"){
      
      m_hat <- m_rot_poly(X = X, Y = Y, c = cutoff, p = 2) 
    }
    
    if (mrot == "poly_3"){
      
      m_hat <- m_rot_poly(X = X, Y = Y, c = cutoff, p = 3) 
    }
    
    if (mrot == "poly_4"){
      
      m_hat <- m_rot_poly(X = X, Y = Y, c = cutoff, p = 4) 
    }
    
    if (mrot == "Imbens"){
      m_hat <- 2 * m_rot_poly(X = X, Y = Y, c = cutoff, p = 2) 
    }
    
    if (mrot == "poly_2_update"){
      rd_params <- data.frame(
        kernel            = kernel,
        ci_method         = ci_method,
        bw_method         = bw_method,
        bw_method_uniform = bw_method_uniform,
        se_method         = se_method,
        alpha             = alpha
      )
      
      m_hat <- m_rot_poly_update(
        X = X, Y = Y, c = cutoff, 
        rd_params = rd_params, 
        first_p = 2, second_p = 2
      ) 
    }
    
    if (mrot == "poly_3_update"){
      rd_params <- data.frame(
        kernel            = kernel,
        ci_method         = ci_method,
        bw_method         = bw_method,
        bw_method_uniform = bw_method_uniform,
        se_method         = se_method,
        alpha             = alpha
      )
      
      m_hat <- m_rot_poly_update(
        X = X, Y = Y, c = cutoff, 
        rd_params = rd_params, 
        first_p = 3, second_p = 3
      ) 
    }
    
    if (mrot == "poly_4_update"){
      rd_params <- data.frame(
        kernel            = kernel,
        ci_method         = ci_method,
        bw_method         = bw_method,
        bw_method_uniform = bw_method_uniform,
        se_method         = se_method,
        alpha             = alpha
      )
      
      m_hat <- m_rot_poly_update(
        X = X, Y = Y, c = cutoff, 
        rd_params = rd_params, 
        first_p = 4, second_p = 4
      ) 
    }
    
    if (mrot == "spline"){
      
      m_hat <- m_rot_spline(
        X = X, Y = Y, c = cutoff
      )
    }
    
    if (mrot == "smooth_spline"){
      
      m_hat <- m_rot_smooth_spline(
        X = X, Y = Y, c = cutoff
      )  
    }
    
    if (mrot == "smooth_spline_update"){
      
      m_hat <- m_rot_smooth_spline_update(
        X = X, Y = Y, c = cutoff
      )  
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
    h_hat             = rd_estimates$h_hat,
    b_hat             = rd_estimates$b_hat,
    m_hat             = m_hat,
    M                 = M
  )
  
  return(coverage_estimates)
}

coverage_prob <- function(
  S,
  data_model,
  n,
  mrot,
  kernel,
  ci_method,
  bw_method,
  bw_method_uniform,
  se_method,
  alpha
){
  
  simulations <- lapply((1:S), 
    function(s){
      coverage_estimates <- coverage_prob_step(
        data_model        = data_model,
        n                 = n,
        kernel            = kernel,
        mrot              = mrot,
        ci_method         = ci_method,
        bw_method         = bw_method,
        bw_method_uniform = bw_method_uniform,
        se_method         = se_method,
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
      tau_hat           = mean(simulations$tau_hat),
      h_hat             = mean(simulations$h_hat),
      b_hat             = mean(simulations$b_hat),
      m_hat             = mean(simulations$m_hat),
      M                 = simulations$M[1]
    )
  
  return(params_and_estimates)
}


