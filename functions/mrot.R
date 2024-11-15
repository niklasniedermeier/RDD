mrot <- function(X, Y, method, c = NULL, p = 3, nknots = 3, show_plot = FALSE) {
  
  if (p == 1) {
    return(0)
  }
  
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
  
  #if (show_plot){
  #  list_df_plot <- list()
  #}
  
  f2_maxima <- c()

  for (i in 1:length(list_df)){
    
     df <- list_df[[i]]
     
     if (method == "poly"){
       
       model      <- lm(df$Y ~ poly(df$X, p, raw = TRUE))
       model_coef <- coef(model)
       
       f2_max_i <- optimize(
         function(x) f2_poly(x, coef = model_coef, p = p), 
         interval = range(df$X), 
         maximum = TRUE
       )$objective
       
       f2_maxima[i] <- f2_max_i
     }
     
     if (method == "Imbens"){
       
       poly_factor = 2
       model      <- lm(df$Y ~ poly(df$X, p, raw = TRUE))
       model_coef <- coef(model)
       
       f2_max_i <- optimize(
         function(x) f2_poly(x, coef = model_coef, p = p), 
         interval = range(df$X), 
         maximum = TRUE
       )$objective
       
       f2_maxima[i] <- poly_factor * f2_max_i
     }
     
     if (method == "smooth_spline"){
       
       model <- npreg::ss(
         x = df$X , y = df$Y,
         method = "BIC",
         nknots = nknots
       )
       
       f2_max_i <- npreg::predict.ss(model,df$X, deriv = 2)$y %>% abs() %>% max()
       
       f2_maxima[i] <-  f2_max_i 
     }
     
     if (method == "spline"){
       
       knot_seq  <- seq(from = min(df$X), to = max(df$X), len = nknots)
       
       X_mat     <- splines2::bSpline(
         x              = df$X, 
         knots          = knot_seq[-c(1, length(knot_seq))], 
         degree         = p,
         intercept      = TRUE, 
         Boundary.knots = knot_seq[ c(1, length(knot_seq))]
       )
       
       beta_hat <- solve(t(X_mat) %*% X_mat)  %*% t(X_mat)  %*% df$Y
       
       X_smooth_length <- min(length(df$X), 100)
       
       X_smooth <- seq(min(df$X),max(df$X), length.out = X_smooth_length)
       
       X_mat_smooth <- splines2::bSpline(
         x              = X_smooth, 
         knots          = knot_seq[-c(1, length(knot_seq))], 
         degree         = p,
         intercept      = TRUE, 
         Boundary.knots = knot_seq[ c(1, length(knot_seq))]
       )
       
       Y_smooth_hat <- X_mat_smooth %*% beta_hat
       
       f2_maxima[i] <- numerical_max_second_derivative(X = X_smooth, Y = Y_smooth_hat)
     }

  }
  
  #if (show_plot){
  #  list_df_plot[[i]] <- data.frame(
  #    X = pred$x,
  #    Y = pred$y
  #  ) %>% arrange(X)
  #}
  
  #if (show_plot){
  #  plot(X, Y, type = "p", col = "black", xlim = c(-1, 1), ylim = c(-1, 2))
  #  for (i in 1:length(list_df)){
  #    lines(list_df_plot[[i]]$X, list_df_plot[[i]]$Y, col = "red")
  #  }
  #  title(main = paste("M = ",round(f2_max,2)))
  #}
  
  f2_max <- round(max(f2_maxima),4)
  
  return(f2_max)
}

f2_poly <- function(x, coef, p) {
  abs(sum(sapply(2:p, function(k) k * (k - 1) * coef[k + 1] * x^(k - 2))))
}



mrot_update <- function(Y, X, method, c, p = 3, nknots = 3, rd_params = NULL, method_update = NULL, runs = 1, show_plot = FALSE) {
  
  if (is.null(method_update)){
    method_update = method
  }
  
  if (is.null(rd_params)){
    rd_params <- data.frame(
      kernel = "triangular",
      ci_method = "honest",
      bw_method = "MSE",
      bw_method_uniform = TRUE,
      se_method = "nn",
      alpha = 0.05
    )
  }
  
  # Get initial m estimate
  m_hat <- mrot(X, Y, method, c, p, nknots, show_plot)
  
  for (i in 1:runs){
    
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
    
    # Adjust Y's below the cutoff using our estimate for tau
    Y[X>c] <- Y[X>c] - tau_hat
    c <- NULL
    m_hat <- mrot(X, Y, method_update, c, p, nknots, show_plot)
  }
  
  return(m_hat)
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




