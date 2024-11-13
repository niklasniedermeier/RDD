# import libs
source(file.path(getwd(), "lib.R"))

# import sources
source(file.path(getwd(), "functions", "coverage_prob.R"))
source(file.path(getwd(), "functions", "dgp.R"))
source(file.path(getwd(), "functions", "mrot.R"))
source(file.path(getwd(), "functions","rd.R"))

# set seed for reproducibility
set.seed(4322)

#-------------------         fix parameters          ---------------------------

S <- 500
alpha <- 0.05

#-----        Analysis of coverage probability and interval length          ----

n <- 100

# Basis for construction of combinations
data_model <- c(
  "design_1",
  "design_2",
  "design_3",
  "design_4"
)

mrot_method <- c(
  "smooth_spline",
  "smooth_spline_update",
  "spline",
  "spline_update",
  "poly_2",
  "poly_3", 
  "poly_4",
  "Imbens",
  "poly_2_update",
  "poly_3_update",
  "poly_4_update"
)

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
  mrot_method = mrot_method,
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
    n                 = as.integer(param$n),
    mrot_method       = as.character(param$mrot_method),
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
  coverage_prob_grid[i,"m_hat"]           <- estimates$m_hat 
  coverage_prob_grid[i,"m"]               <- estimates$M
  
  print(paste0(i," / ", grid_length))
  print(coverage_prob_grid[i,])
  
}


coverage_prob_grid <- coverage_prob_grid %>% dplyr::mutate(
  m_hat_norm = m_hat - m 
)

plotly::plot_ly(
  coverage_prob_grid,
  x = ~m_hat_norm, 
  y = ~data_model, 
  color = ~mrot_method, 
  type = 'scatter'
  , mode = 'markers'
) %>% 
  layout(
    xaxis = list(
      title = "m_hat - m",
      tickvals = seq( round(min(coverage_prob_grid$m_hat_norm))-1  , max(coverage_prob_grid$m_hat_norm) + 1, by = 1)  
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 0, x1 = 0,  # x-position for the vertical line
        y0 = 0, y1 = 4,  # y-range for the line
        line = list(color = "grey", dash = "dot", width = 1)  # Line style: color, dash, width
      )
    ),
    yaxis = list(
      title = "DGP"
    )
  )
  

