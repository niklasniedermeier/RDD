mrot <- function(X, Y, method, c = 0, p = 3, nknots = 3, h = 0.1) {
  
  is_data_valid <- (!is.na(X)) & (!is.na(Y))
  
  X <- X[is_data_valid]
  Y <- Y[is_data_valid]
    
  list_df <- list(
    df_above_c  = data.frame(X = X[X >= c], Y = Y[X >= c]),
    df_below_c  = data.frame(X = X[X <  c], Y = Y[X <  c])
  ) 
 
  f2_maxima <- c()

  for (i in 1:length(list_df)){
    
     df <- list_df[[i]]
     
     if (method == "poly"){
       f2_maxima[i] <- mrot_poly(X = df$X, Y = df$Y, p = p)
     }
     
     if (method == "poly_2_se"){
       f2_maxima[i] <- mrot_poly_2_se(X = df$X, Y = df$Y)
     }
     
     if (method == "ss"){
       f2_maxima[i] <- mrot_ss(X = df$X, Y = df$Y, nknots = nknots)
     }
     
     if (method == "spline"){
       f2_maxima[i] <- mrot_spline(X = df$X, Y = df$Y, nknots = nknots, p = p)
     }
    
     if (method == "locpoly"){
       
       f2_maxima[i] <- mrot_locpoly(X = df$X, Y = df$Y, cutoff = c, h = h, p = p)
     }
     
  }

  
  f2_max <- round(max(f2_maxima),4)
  
  return(f2_max)
}


mrot_poly <- function(X, Y, p){
  
  if (p == 1){return(0)}
  
  model      <- lm(Y ~ poly(X, p, raw = TRUE))
  model_coef <- coef(model)
  
  #cut_border = 0.10
  #if (all(X>=0)){
  #  border_cond <- X <= quantile(X,cut_border)
  #  X = X[border_cond ] 
  #  Y = Y[border_cond ]
  #  
  #}else{
  #  border_cond <- X >= quantile(X,1-cut_border)
  #  X = X[border_cond ] 
  #  Y = Y[border_cond ]
  #}
  
  if (p == 2){
    f2_max <- abs(2 * model_coef[[3]])
  }
  
  if (p == 3){
    f2 <- function(x) abs(2*model_coef[3] + 6*x*model_coef[4])
    f2_max <- max( f2(min(X)), f2(max(X)) )
  }
  
  if (p == 4){
    
    f2 <- function(x) abs(2*model_coef[3]+6*x*model_coef[4]+12*x^2*model_coef[5])
    ## maximum occurs either at endpoints, or else at the extremum,
    ## -r1[4]/(4*r1[5]), if the extremum is in the support
    X_opt <- if (abs(model_coef[5])<=1e-10) Inf else -model_coef[4] / (4*model_coef[5])
    M <- max(f2(min(X)), f2(max(X)))
    if (min(X) < X_opt && max(X) > X_opt) M <- max(f2(X_opt), M)
    f2_max <- M
  }
  
  if (p > 4){
    f2_max <- optimize(
      function(x) f2_poly(x, coef = model_coef, p = p), 
      interval = range(X), 
      maximum = TRUE
    )$objective 
  }
 
  return(f2_max)
}

mrot_poly_2_se <- function(X, Y){
  
  model      <- lm(Y ~ poly(X, p = 2, raw = TRUE))
  model_coef <- coef(model)
  
  f2_max <- abs(2 * model_coef[[3]])
  
  leading_se <- summary(model)$coefficients[, "Std. Error"][[3]]

  delta_upper_ci <- leading_se * 1.96
  f2_max <- f2_max + delta_upper_ci
  
  f2_max <- f2_max #* 1.8
  
  return(f2_max)
}


mrot_ss <- function(X, Y, nknots, p = p){
  model <- npreg::ss(
    x = X , y = Y,
    method = "BIC",
    nknots = nknots
  )
  
  f2_max <- npreg::predict.ss(model, X, deriv = 2)$y %>% abs() %>% max()
  
  return(f2_max)
}


mrot_spline <- function(X, Y, nknots, p){
  
  knot_seq  <- seq(from = min(X), to = max(X), len = nknots)
  
  X_mat     <- splines2::bSpline(
    x              = X, 
    knots          = knot_seq[-c(1, length(knot_seq))], 
    degree         = p,
    intercept      = TRUE, 
    Boundary.knots = knot_seq[ c(1, length(knot_seq))]
  )
  
  beta_hat <- solve(t(X_mat) %*% X_mat)  %*% t(X_mat)  %*% Y
  
  X_smooth_length <- min(length(X), 100)
  
  X_smooth <- seq(min(X), max(X), length.out = X_smooth_length)
  
  X_mat_smooth <- splines2::bSpline(
    x              = X_smooth, 
    knots          = knot_seq[-c(1, length(knot_seq))], 
    degree         = p,
    intercept      = TRUE, 
    Boundary.knots = knot_seq[ c(1, length(knot_seq))]
  )
  
  Y_smooth_hat <- X_mat_smooth %*% beta_hat
  
  
  #cut_border = 1
  #if (all(X_smooth>=0)){
  #  border_cond <- X_smooth <= quantile(X_smooth,cut_border)
  #  X_smooth = X_smooth[border_cond ] 
  #  Y_smooth_hat = Y_smooth_hat[border_cond ]
  #  
  #}else{
  #  border_cond <-  X_smooth >= quantile( X_smooth,1-cut_border)
  #  X_smooth =  X_smooth[border_cond ] 
  #  Y_smooth_hat = Y_smooth_hat[border_cond ]
  #}
  
  f2_max <- numerical_max_second_derivative(X = X_smooth, Y = Y_smooth_hat)
  
  return(f2_max)
}


f2_poly <- function(x, coef, p) {
  abs(sum(sapply(2:p, function(k) k * (k - 1) * coef[k + 1] * x^(k - 2))))
}


numerical_max_second_derivative <- function(X,Y) {
  
  data <- data.frame(X = X, Y = Y)
  
  data <- data %>%
    mutate(
      f2 = (lead(Y) - 2 * Y + lag(Y)) / (lead(X) - X)^2
    ) %>%
    dplyr::filter(!is.na(f2))

  return(max(abs(data$f2)))
}


mrot_lower_bound <- function(X, Y, cutoff, alpha, s, estimate){
  
  rd.honest.fit <- suppressMessages(RDHonest::RDHonest(
    formula       = "Y ~ X", #outcome ~ running_variable 
    data          = data.frame(X = X, Y = Y), 
    cutoff        = cutoff,
    sclass        = "H", # Hölder class
    kern          = "triangular",
    opt.criterion = "MSE",
    se.method     = "nn",
    alpha         = alpha
  ))
  
  m_hat_estimates <- RDHonest::RDSmoothnessBound(
    rd.honest.fit,
    s = s,
    separate = FALSE,
    multiple = TRUE,
    alpha = alpha,
    sclass = "H"
  )
  
  if (estimate == "hb"){
    m_hat <- m_hat_estimates$estimate
  }
  if (estimate == "lower"){
    m_hat <- m_hat_estimates$conf.low 
  }
  
  return(m_hat)
}


mrot_locpoly <- function(X, Y, cutoff, p, h){
  
  r <- KernSmooth::locpoly(
     X,Y,
     drv = 2,
     degree = p,
     kernel = "triangular",
     bandwidth = h
   )
  
  f2_max <- abs(r$y[which.min(abs(r$x - cutoff))])
  
  return(f2_max)
}