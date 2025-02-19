#' Regression Discontinuity Design Estimation
#'
#' Estimates treatment effects in RDD using `RDHonest` or `rdrobust`.
#'
#' @param Y `numeric` Outcome vector
#' @param X `numeric` Running variable vector
#' @param cutoff `numeric` Cutoff point
#' @param M `numeric` Hölder class constant
#' @param kernel `character` Kernel type
#' @param ci_method `character` CI method: `"honest"`, `"conventional"`, `"rbc"`
#' @param bw_method `character` Bandwidth selection method
#' @param bw_method_uniform `logical` Use uniform bandwidth
#' @param se_method `character` SE estimation method
#' @param se_method_J `integer` Nearest neighbors for SE
#' @param alpha `numeric` Significance level
#'
#' @return A `data.frame` with estimatino results. 
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
  se_method_J,
  alpha
){
  # Since RDHonest::RDHonest and rdrobust::rdrobust have different bandwidth
  # name convetions, we define a mapping table which connects equal bandwidth
  # settings
  map_bw_names <- tibble::tribble(
    ~bw_method_uniform,  ~bw_method, ~bw_method_paper,
                  TRUE,       'MSE',            'MSE', 
                  TRUE,      'FLCI',           'FLCI',
                 FALSE,       'MSE',          'mserd',
                 FALSE,        'CE',          'cerrd'
  )
  
  scaleregul = 0
  
  bw_method_mapped <- map_bw_names %>% 
    dplyr::filter(
      .data$bw_method_uniform == .env$bw_method_uniform & .data$bw_method == .env$bw_method
    ) %>%
    dplyr::select("bw_method_paper") %>% 
    dplyr::pull()
  
  # Armstrong and Kolesár
  if (ci_method == "honest" & bw_method_uniform == TRUE){
    
    rd.honest.fit <- RDHonest::RDHonest(
      formula       = "Y ~ X", #outcome ~ running_variable 
      data          = data.frame(X = X, Y = Y), 
      cutoff        = cutoff,
      M             = M,
      sclass        = "H", # Hölder class
      kern          = kernel,
      opt.criterion = bw_method_mapped,
      se.method     = se_method,
      J             = se_method_J,
      alpha         = alpha
    )
    
    tau_hat   <- rd.honest.fit$coefficients$estimate
    se_hat    <- rd.honest.fit$coefficients$std.error
    conf.low  <- rd.honest.fit$coefficients$conf.low
    conf.high <- rd.honest.fit$coefficients$conf.high
    h_hat     <- rd.honest.fit$coefficients$bandwidth
    b_hat     <- NA
    cv        <- rd.honest.fit$coefficients$cv
    pv        <- rd.honest.fit$coefficients$p.value
  }
  
  # Conventional
  if (ci_method == "conventional" & bw_method_uniform == FALSE){
    
    rd.robust.fit <- rdrobust::rdrobust(
      y        = Y, 
      x        = X,
      c        = cutoff,
      p        = 1, #local linear regression,
      kernel   = kernel,
      vce      = se_method,
      nnmatch  = se_method_J,
      level    = (1-alpha)*100,
      bwselect = bw_method_mapped,
      scaleregul = scaleregul
    )
    
    tau_hat   <- rd.robust.fit$coef[1]
    se_hat    <- rd.robust.fit$se[1]
    conf.low  <- rd.robust.fit$ci[1,"CI Lower"]
    conf.high <- rd.robust.fit$ci[1,"CI Upper"]
    h_hat     <- rd.robust.fit$bws[1,1]
    b_hat     <- NA
    cv        <- NA
    pv <- rd.robust.fit$pv[1]
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
      nnmatch  = se_method_J,
      level    = (1-alpha)*100,
      bwselect = bw_method_mapped,
      scaleregul = scaleregul 
    )
    
    tau_hat   <- rd.robust.fit$coef[3]
    se_hat    <- rd.robust.fit$se[3]
    conf.low  <- rd.robust.fit$ci[3,"CI Lower"]
    conf.high <- rd.robust.fit$ci[3,"CI Upper"]
    h_hat     <- rd.robust.fit$bws[1,1]
    b_hat     <- rd.robust.fit$bws[2,1]
    cv        <- NA
    pv        <- rd.robust.fit$pv[3]
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
      J             = se_method_J,
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
      nnmatch  = se_method_J,
      level  = (1-alpha)*100,
      h      = h_hat,
      b      = h_hat
    )
    
    tau_hat   <- rd.robust.fit$coef[3]
    se_hat    <- rd.robust.fit$se[3]
    conf.low  <- rd.robust.fit$ci[3,"CI Lower"]
    conf.high <- rd.robust.fit$ci[3,"CI Upper"]
    h_hat     <- rd.robust.fit$bws[1,1]
    b_hat     <- rd.robust.fit$bws[2,1]
    cv        <- NA
    pv        <- rd.robust.fit$pv[3]
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
    tau_hat    = tau_hat,
    tau_hat_se = se_hat,
    conf.low   = conf.low, 
    conf.high  = conf.high,
    h_hat      = h_hat,
    b_hat      = b_hat,
    cv         = cv,
    pv         = pv
  )
  
  return( cbind(function_arguments, estimation_results) )
}



