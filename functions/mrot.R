mrot <- function(X, Y, method, c = NULL, p = 3, nknots = 3) {
  
  if (is.null(c)){
    list_df <- list(
      df_all  = data.frame(X = X, Y = Y)
    )
  }else{
    list_df <- list(
      df_above_c  = data.frame(X = X[X >= c], Y = Y[X >= c]),
      df_below_c  = data.frame(X = X[X <  c], Y = Y[X <  c])
    ) 
  }
  
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

  }

  
  f2_max <- round(max(f2_maxima),4)
  
  return(f2_max)
}


mrot_poly <- function(X, Y, p){
  
  if (p == 1){return(0)}
  
  model      <- lm(Y ~ poly(X, p, raw = TRUE))
  model_coef <- coef(model)
  
  if (p == 2){
    f2_max <- abs(2 * model_coef[[3]])
  }
  
  if (p > 2){
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
  
  f2_max <- numerical_max_second_derivative(X = X_smooth, Y = Y_smooth_hat)
  
  return(f2_max)
}


f2_poly <- function(x, coef, p) {
  abs(sum(sapply(2:p, function(k) k * (k - 1) * coef[k + 1] * x^(k - 2))))
}


numerical_max_second_derivative <- function(X,Y) {
  
  # Create a data frame for X and Y
  data <- data.frame(X = X, Y = Y)
  
  # Compute second derivative using central difference
  data <- data %>%
    mutate(
      f2 = (lead(Y) - 2 * Y + lag(Y)) / (lead(X) - X)^2
    ) %>%
    dplyr::filter(!is.na(f2))

  return(max(abs(data$f2)))
}


optimize_polynomial_degree <- function(x, y, max_degree = 10, n_folds = 5) {
  
  data <- data.frame(x = x, y = y)
  
  # Define cross-validation control
  train_control <- trainControl(method = "cv", number = n_folds)
  
  cv_results <- data.frame(degree = integer(0), RMSE = numeric(0))
  
  for (k in 1:max_degree) {
    # Define the formula for the polynomial regression model
    formula <- as.formula(paste("y ~ poly(x, ", k, ", raw = TRUE)", sep = ""))
    
    # Fit the model with cross-validation
    model <- train(formula, data = data, method = "lm", trControl = train_control)
    
    # Store the results
    cv_results <- rbind(cv_results, data.frame(degree = k, RMSE = model$results$RMSE))
  }
  
  # Select the degree with the lowest RMSE
  optimal_degree <- cv_results$degree[which.min(cv_results$RMSE)]
  
  return(optimal_degree)
}




