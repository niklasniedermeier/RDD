m_rot_poly <- function(X, Y, c, p) {
  
  if (p == 1) {
    return(0)
  }
  
  list_df <- list(
    df_above_c  = data.frame(X = X[X >= c], Y = Y[X >= c]),
    df_below_c  = data.frame(X = X[X <  c], Y = Y[X <  c])
  )
  
  f2 <- function(x, coef) {
    abs(sum(sapply(2:p, function(k) k * (k - 1) * coef[k + 1] * x^(k - 2))))
  }
  
  f2_maxima <- c()

  for (i in 1:length(list_df)){
  
     df <- list_df[[i]]
     # Estimate global polynomial regression
     model      <- lm(df$Y ~ poly(df$X, p, raw = TRUE))
     model_coef <- coef(model)
     # Estimate maximum of absolute second derivative
     f2_maxima[i] <- optimize(
       function(x) f2(x, coef = model_coef), 
       interval = range(df$X), 
       maximum = TRUE
      )$objective
  }
  
  f2_max <- max(f2_maxima)

  return(f2_max)
}


m_rot_poly_update <- function(Y, X, c, rd_params, first_p, second_p) {
  
  if (first_p == 1) {
    return(0)
  }
  
  #-----------------------------------------------------------------------------
  # Get initial m estimate
  first_m_hat <- m_rot_poly(X = X, Y = Y, c = c, p = first_p) 
  
  #-----------------------------------------------------------------------------
  # Get RD estimates for initial tau
  rd_estimates <- suppressMessages(
    rd(
      Y = Y,
      X = X,
      cutoff = c,
      M = first_m_hat,
      kernel = rd_params$kernel,
      ci_method = rd_params$ci_method,
      bw_method = rd_params$bw_method,
      bw_method_uniform = rd_params$bw_method_uniform,
      se_method = rd_params$se_method,
      alpha = rd_params$alpha
    )
  )
  
  tau_hat <- rd_estimates$tau_hat
  #-----------------------------------------------------------------------------
  # Adjust Y's below the cutoff using our estimnate for tau
  Y[X<c] <- Y[X<c] + tau_hat
  
  f2 <- function(x, coef) {
    abs(sum(sapply(2:second_p, function(k) k * (k - 1) * coef[k + 1] * x^(k - 2))))
  }
  
  # Estimate global polynomial regression
  model      <- lm(Y ~ poly(X, second_p, raw = TRUE))
  model_coef <- coef(model)
  # Estimate maximum of absolute second derivative
  f2_max <- optimize(
    function(x) f2(x, coef = model_coef), 
    interval = range(X), 
    maximum = TRUE
  )$objective
  
  return(f2_max)
}
