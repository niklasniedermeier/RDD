dgp <- function(data_model, n, M){
  
  # Running variable
  X <- runif(n, -1, 1)
  
  # Jump & Cutoff
  tau = 0.25
  cutoff = 0
  
  # Noise
  noise <- rnorm(n, mean = 0, sd = sqrt(0.25))  
  
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
  if(data_model == "test"){
    f <- f_design_6(X, M)
  } 
  # Observed outcome
  Y <- f + noise + (X>=cutoff) * tau
  
  data <- list(Y = Y, X = X, tau = tau, cutoff = cutoff, M = M)
  
  return(data)
}


f_design_1 <- function(X, M){
  f <- sapply(X, function(x){
    0.5* M * 
      (
        (x)^2 
        - 2 * max(abs(x) - 0.2, 0)^2 
        + 2 * max(abs(x) - 0.5, 0)^2 
        - 2 * max(abs(x) - 0.65, 0)^2 
      )  
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
        - 0.5
      )  
  }
  )
  return(f)
} 


f_design_4 <- function(X, M){
  f <- sapply(X, function(x){
    0.5 * M * 
      (
        (x+1)^2                 # x^2             ->     2x   ->   2   
        - 2 * max(x, 0)^2       # x^2 -2x^2       ->    -2x   ->  -2 
        - 4 * max(x - 0.6, 0)^2 # x^2 -2x^2 -4x^2 ->   -10x   -> -10
        - 0.5
      )  
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


f_design_6 <- function(X, M){
  
  f <- sapply(X, function(x){
    0.5 * M  * 
      (
        x^2 
        - 2 * max(abs(x) - 0.25, 0)^2
      )
  }
  )
  return(f)
} 

show_dgp <- function(data_model){
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
  
  line = list(width = 1.5) 
  
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
      legend = list(x = 0.07, y = 1.035)
  )
}


show_dgp()
