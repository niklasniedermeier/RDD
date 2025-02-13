#' ROTs to estimate the Smoothness Bound. 
#'
#' @param X `numeric` Running variable vector
#' @param Y `numeric` Outcome vector
#' @param method `character` Method to estimate the second derivative: `"poly"`, `"poly_2_se"`, or `"spline"`
#' @param c `numeric` Cutoff point, default is 0
#' @param p `integer` Polynomial degree, default is 3
#' @param nknots `integer` Number of knots for spline, default is 3
#'
#' @return `numeric` Maximum estimated second derivative for the selected method
mrot <- function(X, Y, method, c = 0, p = 3, nknots = 3) {
  
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
     
     
     if (method == "spline"){
       f2_maxima[i] <- mrot_spline(X = df$X, Y = df$Y, nknots = nknots, p = p)
     }
  }

  
  f2_max <- round(max(f2_maxima),4)
  
  return(f2_max)
}


#' Polynomial ROT
#'
#' @param X `numeric` Running variable vector
#' @param Y `numeric` Outcome vector
#' @param p `numeric` Polynomial degree
#'
#' @return `numeric` Maximum estimated second derivative based on polynomial fit
mrot_poly <- function(X, Y, p){
  
  if (p == 1){return(0)}
  
  model      <- lm(Y ~ poly(X, p, raw = TRUE))
  model_coef <- coef(model)
  
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


#' Polynomial ROT using q = 2 with "heuristic upperbound"
#'
#' @param X `numeric` Running variable vector
#' @param Y `numeric` Outcome vector
#'
#' @return `numeric` Maximum estimated second derivative based on polynomial fit
mrot_poly_2_se <- function(X, Y){
  
  model      <- lm(Y ~ poly(X, p = 2, raw = TRUE))
  model_coef <- coef(model)
  
  f2_max <- abs(2 * model_coef[[3]])
  
  leading_se <- summary(model)$coefficients[, "Std. Error"][[3]]

  delta_upper_ci <- leading_se * 1.96
  f2_max <- f2_max + delta_upper_ci
  
  return(f2_max)
}


#' Spline ROT 
#'
#' @param X `numeric` Running variable vector
#' @param Y `numeric` Outcome vector
#' @param nknots `integer` Number of knots. 
#' @param p `integer` Polynomial degree. 
#'
#' @return `numeric` Maximum estimated second derivative based on spline fit
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
  
  f2_max <- numerical_max_second_derivative(X = X_smooth, Y = Y_smooth_hat)
  
  return(f2_max)
}


#' Second Derivative of Polynomial
#'
#' Computes the absolute second derivative of a polynomial at a given point.
#'
#' @param x `numeric` Point at which to evaluate the second derivative
#' @param coef `numeric` Vector of polynomial coefficients
#' @param p `numeric` Polynomial degree
#'
#' @return `numeric` Absolute value of the second derivative at `x`
f2_poly <- function(x, coef, p) {
  abs(sum(sapply(2:p, function(k) k * (k - 1) * coef[k + 1] * x^(k - 2))))
}


#' Numerical Maximum Second Derivative
#'
#' Estimates the maximum second derivative numerically using finite differences.
#'
#' @param X `numeric` Running variable vector
#' @param Y `numeric` Outcome vector
#'
#' @return `numeric` Maximum absolute second derivative computed from the data
numerical_max_second_derivative <- function(X,Y) {
  
  data <- data.frame(X = X, Y = Y)
  
  data <- data %>%
    mutate(
      f2 = (lead(Y) - 2 * Y + lag(Y)) / (lead(X) - X)^2
    ) %>%
    dplyr::filter(!is.na(f2))

  return(max(abs(data$f2)))
}


#' Lower Bound ROT
#'
#' @param X `numeric` Running variable vector
#' @param Y `numeric` Outcome vector
#' @param cutoff `numeric` Cutoff
#' @param alpha `numeric` Significance level
#' @param s `integer` Number of neighbors 
#' @param estimate `character` Either lower bound `lower` or half biased estimate `hb`
#'
#' @return `numeric` Maximum estimated second derivative based on spline fit
mrot_lower_bound <- function(X, Y, cutoff, alpha, s, estimate){
  
  rd.honest.fit <- suppressMessages(RDHonest::RDHonest(
    formula       = "Y ~ X", 
    data          = data.frame(X = X, Y = Y), 
    cutoff        = cutoff,
    sclass        = "H", 
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

