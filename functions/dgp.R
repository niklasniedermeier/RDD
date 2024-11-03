dgp <- function(data_model, n, M){
  
  if(data_model == "m1"){
    
    # Model 1: Armstrong & Kolesar 
    
    # Running variable
    X <- runif(n, -1, 1)
    
    # Jump & Cutoff
    tau = 1
    cutoff = 0
    
    # Underlying function
    f <- 0.5 * M * (X^2 - 2 * max( abs(X) - 0.25, 0 ) )
    
    # Noise
    noise <- rnorm(n, mean = 0, sd = 0.25)  
    
    # Observed outcome
    Y <- f + noise + (X>=cutoff) * tau
    
  }
  
  if(data_model == "m2"){
    
    # Running variable
    X <- runif(n, -1, 1)
    
    # Jump & Cutoff
    tau = 2
    cutoff = 0
    
    # Underlying function
    M <- 2
    f <- 5+3*X
    
    # Noise
    noise <- rnorm(n, mean = 0, sd = 0.25)  
    
    # Observed outcome
    Y <- f + noise + (X>=cutoff) * tau
    
  }
  
  data <- list(Y = Y, X = X, tau = tau, cutoff = cutoff)
  
  return(data)
}

