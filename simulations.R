# import sources
source(file.path(getwd(),"functions" ,"coverage_prob.R"))
source(file.path(getwd(),"functions" ,"dgp.R"))
source(file.path(getwd(),"functions" ,"mrot.R"))
source(file.path(getwd(),"functions" ,"rd.R"))

# set seed for reproducibility
set.seed(4322)

#-------------------         fix parameters          ---------------------------

S <- 300
alpha <- 0.05

#-----        Analysis of coverage probability and interval length          ----

# Basis for construction of combinations
data_model <- c("m1")
data_model_m <- c(1,2,3,4,5)
mrot <- c(
  #"Armstrong_and_Kolesar", "Imbens_and_Wager", 
  "poly_2",
  "poly_3",
  "poly_4",
  "poly_update_2",
  "poly_update_3",
  "poly_update_4"
  )
n <- 500
kernel <- c("triangular")

# Methods with uniform bandwidth
uniform_params <- tibble::tribble(
  ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
   "honest",      "MSE",               TRUE,      "nn",
   #"honest",     "FLCI",               TRUE,      "nn",
   #   "rbc",      "MSE",               TRUE,      "nn"
  
) 

uniform_grid <- expand.grid(
  n = n,
  data_model = data_model,
  data_model_m = data_model_m,
  mrot = mrot,
  kernel = kernel
)
  

coverage_prob_grid <- cbind(uniform_params, uniform_grid)


#rbc_params <- tibble::tribble(
#  ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
#      "rbc",      "MSE",               FALSE,      "nn",
#      "rbc",       "CE",               FALSE,      "nn",
#)

grid_length <- nrow(coverage_prob_grid)

# Estimate coverage probability for all combinations
for (i in c(1:grid_length)){
  
  param <- coverage_prob_grid[i,]
  
  estimates <- coverage_prob( 
    S                 = S,
    data_model        = as.character(param$data_model),
    data_model_m      = as.integer(param$data_model_m),
    n                 = as.integer(param$n),
    mrot              = as.character(param$mrot),
    kernel            = as.character(param$kernel),
    ci_method         = as.character(param$ci_method),
    bw_method         = as.character(param$bw_method),
    bw_method_uniform = as.logical(param$bw_method_uniform),
    se_method         = as.character(param$se_method),
    alpha             = alpha
  )
  
  coverage_prob_grid[i,"coverage_prob"]   <- estimates$coverage_prob
  coverage_prob_grid[i,"interval_length"] <- estimates$interval_length
  coverage_prob_grid[i,"tau_hat"]         <- estimates$tau_hat
  coverage_prob_grid[i,"h_hat"]           <- estimates$h_hat
  coverage_prob_grid[i,"b_hat"]           <- estimates$b_hat 
  coverage_prob_grid[i,"m_hat"]          <- estimates$m_hat 
  
  print(paste0(i," / ", grid_length))
  print(coverage_prob_grid[i,])
  
}


plotly::plot_ly(
  bind_rows(
    coverage_prob_grid, 
    data.frame(data_model_m = c(1:5), m_hat = c(1:5), mrot = "optimal")
    ),
  x = ~data_model_m, 
  y = ~m_hat, 
  color = ~mrot, 
  type = 'scatter', 
  mode = 'lines'
  ) 

plotly::plot_ly(
  coverage_prob_grid,
  x = ~data_model_m, 
  y = ~interval_length, 
  color = ~mrot, 
  type = 'scatter', 
  mode = 'lines'
) 


