dgp <- function(data_model, n, M, noise_method){
  
  X <- runif(n, -1, 1)
  
  tau = 0.25
  cutoff = 0
  
  noise <- get_noise(X = X, noise_method = noise_method)
  
  f <- get_f(X = X, M = M, data_model = data_model)

  Y <- f + noise + (X>=cutoff) * tau
  
  #Y <- f + noise + (X>=cutoff) * tau *(1 + X)^2
  
  data <- list(Y = Y, X = X, tau = tau, cutoff = cutoff, M = M)
  
  return(data)
}


get_noise <- function(X, noise_method){
  
  n <- length(X)
  
  if (noise_method ==  "homoscedastic"){
    var <- 0.25
  }
  
  if (noise_method == "cluster_2"){
    group_size <- 2
    groups <- cut(X, breaks = group_size, labels = FALSE)
    var_groups <- seq(0.5,2.5, length.out = group_size)
    var <-  var_groups[groups]
  }
  
  if (noise_method == "linear"){
    var <- 0.25*(1+X)
  }
  
  if (noise_method == "quadratic"){
    var <- 0.25*(1+X)^2
  }
  
  if (noise_method == "cubic"){
    var <- 0.25*(1+X)^3
  }
  
  if (noise_method == "local") {
    sigma_base <- 0.1
    sigma_peak <- 3
    k <- 5  # Controls the width of the peak
    var<- sigma_base + sigma_peak * exp(-k * X^2)
  }
  
  if (noise_method == "border") {
    var<- 0.25*(1+sqrt(abs(X)))^2
  }
  
  noise <- rnorm(n, mean = 0, sd = sqrt(var))
  
  return(noise)
}


get_f <- function(X, M, data_model){
  
  if(data_model == "design_1"){
    f <- f_design_1(X, M)
  } 
  if(data_model == "design_2"){
    f <- f_design_2(X, M)
  } 
  if(data_model == "design_3"){
    f <- f_design_3(X, M)
  }
  if(data_model == "design_4"){
    f <- f_design_4(X, M)
  } 
  if(data_model == "design_5"){
    f <- f_design_5(X, M)
  } 
  
  return(f)
}


f_design_1 <- function(X, M){
  f <- sapply(X, function(x){
    0.5* M * 
      (
        (x)^2 
        - 2 * max(abs(x) - 0.2, 0)^2 
        + 2 * max(abs(x) - 0.5, 0)^2 
        - 2 * max(abs(x) - 0.65, 0)^2 
      )  + 0.1
  }
  )
  return(f)
} 


f_design_2 <- function(X, M){
  
  f <- sapply(X, function(x){
    0.5 * M  * 
      (
        (x + 1)^2 
        - 2 * max(x + 0.4, 0)^2
        + 2 * max(x - 0.6, 0)^2

      )
  }
  )
  return(f)
} 


f_design_3 <- function(X, M){
  f <- sapply(X, function(x){
    0.5 * M * 
      (
        (x+1)^2
        - 2 * max(x + 0.6, 0)^2 
        + 2 * max(x + 0.4, 0)^2 
        - 2 * max(x, 0)^2 
        + 2 * max(x - 0.4, 0)^2 
        - 2 * max(x - 0.6, 0)^2 
      ) - 0.5
  
  }
  )
  return(f)
} 


f_design_4 <- function(X, M){
  f <- sapply(X, function(x){
    0.5 * M * 
      (
        (x+0.3)^2                 
        - 2 * max(x, 0)^2   
        + 2 * max(x-0.3,0)^2
      )  -0.5
  }
  )
  return(f)
} 


f_design_5 <- function(X, M){
  f <- sapply(X, function(x){
    (1/2) * M * 
      ifelse(x>=0,-x^2,x^2)
  }
  )
  return(f)
} 


show_dgp <- function(){
  n <- 10000
  X <- runif(n, -1, 1)

  X <- sort(X)
  
  M <- 4

  data <- data.frame(
    X = X,
    design_1 = f_design_1(X, M), 
    design_2 = f_design_2(X, M),
    design_3 = f_design_3(X, M),
    design_4 = f_design_4(X, M),
    design_5 = f_design_5(X, M)
  )
  
  line = list(width = 2) 
  
  plotly::plot_ly(data, x = ~X) %>% 
    add_trace(y = ~design_1, line = line, name = 'Design: 1', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~design_2, line = line, name = 'Design: 2', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~design_3, line = line, name = 'Design: 3', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~design_4, line = line, name = 'Design: 4', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~design_5, line = line, name = 'Design: 5', type = 'scatter', mode = 'lines') %>%
    layout(
      xaxis = list(
        title = "x",
        tickvals = c(-1.0, -0.5, 0.0, 0.5, 1.0)
      ), 
      yaxis = list(
        title = 'f(x)',
        tickvals = c(-2.0,-1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5,2.0,2.5,3.0)
      ), 
      legend = list(x = 0.25, y = 0.05)
  )
}


show_dgp_single_all_m <- function(design_num){
  n <- 10000
  X <- runif(n, -1, 1)
  
  X <- sort(X)
  
  f_design <- get(paste0("f_design_", design_num))
  
  data <- data.frame(
    X = X,
    M_4 = f_design(X, 4),
    M_8 = f_design(X, 8)
  )
  
  line = list(width = 1.5) 
  
  plotly::plot_ly(data, x = ~X) %>% 
    add_trace(y = ~M_4, line = line, name = 'M: 4', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~M_8, line = line, name = 'M: 8', type = 'scatter', mode = 'lines') %>%
    layout(
      xaxis = list(
        title = "x"
      ), 
      yaxis = list(
        title = 'f(x)'
      ), 
      legend = list(x = 0.07, y = 1.035)
    )
}

#show_dgp_single_all_m(1) 
#show_dgp_single_all_m(2) 
#show_dgp_single_all_m(3) 
#show_dgp_single_all_m(4) 
#show_dgp_single_all_m(5) 

show_dgp()
