rd <- function(
  Y,
  X,
  cutoff,
  M,
  kernel,
  ci_method,
  bw_method,
  bw_method_uniform,
  se_method,
  alpha
){
  # Since RDHonest::RDHonest and rdrobust::rdrobust have different bandwidth
  # name convetions, we define a mapping table which connects equal bandwidth
  # settings
  map_bw_names <- tibble::tribble(
    ~bw_method_uniform,  ~bw_method, ~bw_method_paper,
                  TRUE,       'MSE',            'MSE', 
                  TRUE,      'FLCI',           'FLCI',
                 FALSE,     'mserd',          'mserd',
                 FALSE,     'cerrd',          'cerrd'
  )
  
  bw_method_mapped <- map_bw_names %>% 
    dplyr::filter(
      .data$bw_method_uniform == .env$bw_method_uniform & .data$bw_method == .env$bw_method
    ) %>%
    dplyr::select("bw_method_paper") %>% 
    dplyr::pull()
  
  # Armstrong and Kolesár
  if (ci_method == "honest" & bw_method_uniform == TRUE){
    
    rd.honest.fit <- RDHonest::RDHonest(
      formula       = "Y ~ X", 
      data          = data.frame(X = X, Y = Y), 
      cutoff        = cutoff,
      M             = M,
      sclass        = "H", # Hölder class
      kern          = kernel,
      opt.criterion = bw_method_mapped,
      se.method     = se_method,
      alpha         = alpha
    )
    
    tau_hat   <- rd.honest.fit$coefficients$estimate
    se_hat    <- rd.honest.fit$coefficients$std.error
    conf.low  <- rd.honest.fit$coefficients$conf.low
    conf.high <- rd.honest.fit$coefficients$conf.high
    h_hat     <- rd.honest.fit$coefficients$bandwidth
    b_hat     <- NA
  }
  
  # Calonico, Cattaneo, Farrell and Titiunik
  if (ci_method == "rbc" & bw_method_uniform == FALSE){
    
    rd.robust.fit <- rdrobust::rdrobust(
      y        = Y, 
      x        = X,
      c        = cutoff,
      p        = 1, #local linear regression,
      q        = 2, #bias estimation using local quadratic regression
      kernel   = kernel,
      vce      = se_method,
      level    = (1-alpha)*100,
      bwselect = bw_method_mapped 
    )
    
    tau_hat   <- rd.robust.fit$Estimate[1,"tau.bc"]
    se_hat    <- rd.robust.fit$Estimate[1,"se.rb"]
    conf.low  <- rd.robust.fit$ci[3,"CI Lower"]
    conf.high <- rd.robust.fit$ci[3,"CI Upper"]
    h_hat     <- rd.robust.fit$bws[1,1]
    b_hat     <- rd.robust.fit$bws[2,1]
  }
  
  # Combination of Armstrong et al and Calonico et al
  if (ci_method == "rbc" & bw_method_uniform == TRUE){
    
    # Using Armstrong and Kolesár's method to get a uniform optimized bandwidth
    rd.honest.fit <- RDHonest::RDHonest(
      formula       = "Y ~ X", 
      data          = data.frame(X = X, Y = Y), 
      cutoff        = cutoff,
      M             = M,
      sclass        = "H", # Hölder class
      kern          = kernel,
      opt.criterion = bw_method_mapped,
      se.method     = se_method,
      alpha         = alpha
    )
    
    h_hat <- rd.honest.fit$coefficients$bandwidth
    
    # Using Calonico et al to perform robust bias correction
    rd.robust.fit <- rdrobust::rdrobust(
      y      = Y, 
      x      = X,
      c      = cutoff,
      p      = 1, #local linear regression,
      q      = 2, #bias estimation using local quadratic regression
      kernel = kernel,
      vce    = se_method,
      level  = (1-alpha)*100,
      h      = h_hat,
      b      = h_hat
    )
    
    tau_hat   <- rd.robust.fit$Estimate[1,"tau.bc"]
    se_hat    <- rd.robust.fit$Estimate[1,"se.rb"]
    conf.low  <- rd.robust.fit$ci[3,"CI Lower"]
    conf.high <- rd.robust.fit$ci[3,"CI Upper"]
    h_hat     <- rd.robust.fit$bws[1,1]
    b_hat     <- rd.robust.fit$bws[2,1]
  }
  
  function_arguments <- data.frame(
    cutoff            = cutoff,
    M                 = M,
    kernel            = kernel,
    ci_method         = ci_method,
    bw_method         = bw_method,
    bw_method_uniform = bw_method_uniform,
    se_method         = se_method,
    alpha             = alpha
  )
  
  estimation_results <- data.frame(
    tau_hat   = tau_hat,
    se_hat    = se_hat,
    conf.low  = conf.low, 
    conf.high = conf.high,
    h_hat     = h_hat,
    b_hat     = b_hat  
  )
  return( cbind(function_arguments, estimation_results) )
}

