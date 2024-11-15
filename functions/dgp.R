dgp <- function(data_model, n){
  
  # Running variable
  X <- runif(n, -1, 1)
  
  # Jump & Cutoff
  tau = 1
  cutoff = 0
  
  # Noise
  noise <- rnorm(n, mean = 0, sd = 0.25)  
  
  if(data_model == "design_1"){
    M <- 1
    f <- f_design_1(X, M)
  } 
  if(data_model == "design_2"){
    M <- 9
    f <- f_design_2(X, M)
  } 
  if(data_model == "design_3"){
    M <- 3
    f <- f_design_3(X, M)
  }
  if(data_model == "design_4"){
    M <- 10
    f <- f_design_4(X, M)
  } 
  
  # Observed outcome
  Y <- f + noise + (X>=cutoff) * tau
  
  data <- list(Y = Y, X = X, tau = tau, cutoff = cutoff, M = M)
  
  return(data)
}


f_design_1 <- function(X, M){
  f <- sapply(X, function(x){
    0.5 * M * 
      (
        (x+1)^2
        - 2 * max(x + 0.3, 0)^2 
        + 2 * max(x - 0.2, 0)^2 
        - 2 * max(x - 0.7, 0)^2 
        - 0.5
      )  
    }
  )
  return(f)
} 

f_design_2 <- function(X, M){
  # from Armstrong et al. 
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

f_design_3 <- function(X, M){
  
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


f_design_4 <- function(X, M){
  f <- sapply(X, function(x){
    (1/3) * 0.5 * M * 
      (
        (x + 0.05)^2 
        - 2 * max(x, 0)^2 
        + 2 * max(x - 0.4, 0)^2 
        + 2 * max(x - 0.5, 0)^2 
        - 2 * max(x - 0.6, 0)^2 
        - 0.3
      )  
  }
  )
  return(f)
} 

show_dgp <- function(data_model){
  n <- 1000
  X <- runif(n, -1, 1)

  X <- sort(X)
  
  data <- data.frame(
    X = X,
    design_1 = f_design_1(X, 1),
    design_2 = f_design_2(X, 9),
    design_3 = f_design_3(X, 3),
    design_4 = f_design_4(X, 10)
  )
  
  plotly::plot_ly(data, x = ~X) %>% 
    add_trace(y = ~design_1, name = 'design_1', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~design_2, name = 'design_2', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~design_3, name = 'design_3', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~design_4, name = 'design_4', type = 'scatter', mode = 'lines') %>%
    layout(xaxis = list(title = "x"), yaxis = list(title = 'f(x)'), title = "DGP's")
 }
