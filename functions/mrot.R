m_rot_poly <- function(X, Y, c, p) {
  
  if (p == 1) {
    return(0)
  }
  
  list_df <- list(
    df_above_c  = data.frame(X = X[X >= c], Y = Y[X >= c]),
    df_below_c  = data.frame(X = X[X <  c], Y = Y[X <  c])
  )
  
  f2_maxima <- c()

  for (i in 1:length(list_df)){
  
     df <- list_df[[i]]
     # Estimate global polynomial regression
     model      <- lm(df$Y ~ poly(df$X, p, raw = TRUE))
     model_coef <- coef(model)
     # Estimate maximum of absolute second derivative
     f2_maxima[i] <- optimize(
       function(x) f2_poly(x, coef = model_coef, p = p), 
       interval = range(df$X), 
       maximum = TRUE
      )$objective
  }
  
  f2_max <- max(f2_maxima)

  return(f2_max)
}

f2_poly <- function(x, coef, p) {
  abs(sum(sapply(2:p, function(k) k * (k - 1) * coef[k + 1] * x^(k - 2))))
}

m_rot_poly_update <- function(Y, X, c, first_p, second_p, rd_params, runs = 1) {
  
  if (first_p == 1 | second_p == 1) {
    return(0)
  }
  
  if (missing(rd_params)){
    rd_params <- data.frame(
      kernel = "triangular",
      ci_method = "honest",
      bw_method = "MSE",
      bw_method_uniform = TRUE,
      se_method = "nn",
      alpha = 0.05
    )
  }
  #-----------------------------------------------------------------------------
  # Get initial m estimate
  m_hat <- m_rot_poly(X = X, Y = Y, c = c, p = first_p) 
  
  for (i in 1:runs){
    
    #-----------------------------------------------------------------------------
    # Get RD estimates for initial tau
    rd_estimates <- suppressMessages(
      rd(
        Y = Y,
        X = X,
        cutoff = c,
        M = m_hat,
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
    # Adjust Y's below the cutoff using our estimate for tau
    Y[X>c] <- Y[X>c] - tau_hat
    
    # Estimate global polynomial regression
    model      <- lm(Y ~ poly(X, second_p, raw = TRUE))
    model_coef <- coef(model)
    
    # Estimate maximum of absolute second derivative
    f2_max <- optimize(
      function(x) f2_poly(x, coef = model_coef, p = second_p), 
      interval = range(X), 
      maximum = TRUE
    )$objective
    
    m_hat <- f2_max
  }
  
  return(m_hat)
}


m_rot_spline <- function(X, Y, c, q = 2, k = 3) {
  
  list_df <- list(
    df_above_c  = data.frame(X = X[X >= c], Y = Y[X >= c]),
    df_below_c  = data.frame(X = X[X <  c], Y = Y[X <  c])
  )
  
  f2_maxima <- c()
  
  for (i in 1:length(list_df)){
    
    df <- list_df[[i]]
    
    knot_seq  <- seq(from = min(df$X), to = max(df$X), len = q)
    
    X_mat     <- splines2::bSpline(
      x              = df$X, 
      knots          = knot_seq[-c(1, length(knot_seq))], 
      degree         = k,
      intercept      = TRUE, 
      Boundary.knots = knot_seq[ c(1, length(knot_seq))]
    )
    
    beta_hat <- solve(t(X_mat) %*% X_mat)  %*% t(X_mat)  %*% df$Y
    
    X_smooth_length <- min(length(df$X),100)
    
    X_smooth <- seq(min(df$X),max(df$X), length.out = X_smooth_length)
    
    X_mat_smooth <- splines2::bSpline(
      x              = X_smooth, 
      knots          = knot_seq[-c(1, length(knot_seq))], 
      degree         = k,
      intercept      = TRUE, 
      Boundary.knots = knot_seq[ c(1, length(knot_seq))]
    )
    
    Y_smooth_hat <- X_mat_smooth %*% beta_hat
    
    f2_maxima[i] <- numerical_max_second_derivative(X = X_smooth, Y = Y_smooth_hat)
    
  }
  
  f2_max <- max(f2_maxima)
  
  return(f2_max)
}

m_rot_smooth_spline <- function(X, Y, c, show_plot = FALSE) {
  
  list_df <- list(
    df_above_c  = data.frame(X = X[X >= c], Y = Y[X >= c]),
    df_below_c  = data.frame(X = X[X <  c], Y = Y[X <  c])
  )
  
  if (show_plot){
    list_df_plot <- list()
  }

  f2_maxima <- c()
  
  for (i in 1:length(list_df)){
    
    df <- list_df[[i]]
    
    model <- npreg::ss(
      x = df$X , y = df$Y,
      method = "OCV",
      nknots = 3
    )
    
    X_smooth_length <- max(length(df$X), 100)
    
    X_smooth <- seq(min(df$X), max(df$X), length.out = X_smooth_length)
    
    pred <- predict(model, X_smooth)
    
    f2_maxima[i] <- numerical_max_second_derivative(X = pred$x, Y = pred$y)
    
    if (show_plot){
      list_df_plot[[i]] <- data.frame(
        X = pred$x,
        Y = pred$y
      ) %>% arrange(X)
    }
  }
  
  f2_max <- max(f2_maxima)
  
  if (show_plot){
    plot(X, Y, type = "p", col = "black", xlim = c(-1, 1), ylim = c(-1, 2))
    lines(list_df_plot[[1]]$X, list_df_plot[[1]]$Y, col = "red")
    lines(list_df_plot[[2]]$X, list_df_plot[[2]]$Y, col = "red")
    title(main = paste("M = ",round(f2_max,2)))
  }
  
  return(f2_max)
}

m_rot_smooth_spline_update <- function(Y, X, c, rd_params, runs = 1) {
  
  if (missing(rd_params)){
    rd_params <- data.frame(
      kernel = "triangular",
      ci_method = "honest",
      bw_method = "MSE",
      bw_method_uniform = TRUE,
      se_method = "nn",
      alpha = 0.05
    )
  }
  #-----------------------------------------------------------------------------
  # Get initial m estimate
  m_hat <- m_rot_smooth_spline(X = X, Y = Y, c = c) 
  
  for (i in 1:runs){
    
    #-----------------------------------------------------------------------------
    # Get RD estimates for initial tau
    rd_estimates <- suppressMessages(
      rd(
        Y = Y,
        X = X,
        cutoff = c,
        M = m_hat,
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
    # Adjust Y's below the cutoff using our estimate for tau
    Y[X>c] <- Y[X>c] - tau_hat
    
    model <- npreg::ss(
      x = X , y = Y,
      method = "OCV",
      nknots = 3 + 2
    )
    
    X_smooth_length <- max(length(X), 100)
    
    X_smooth <- seq(min(X), max(X), length.out = X_smooth_length)
    
    pred <- predict(model, X_smooth)
    
    f2_max <- numerical_max_second_derivative(X = pred$x, Y = pred$y)
    
    m_hat <- f2_max
  }
  
  return(m_hat)
}

numerical_max_second_derivative <- function(X,Y){
  
  #Assuming X and Y have at least 3 elements
  Y_diff2 <- diff(Y, differences = 2)
  X_diff1 <- diff(X, differences = 1)
  
  # Adjust the length of the vectors to be compatible
  second_derivative <- Y_diff2 / (X_diff1[2:length(X_diff1)]^2)  
  
  f2_max <- max(abs(second_derivative))
  
  return(f2_max)
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




