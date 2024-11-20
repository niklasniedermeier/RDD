# import libs
source(file.path(getwd(), "lib.R"))

# import sources
source(file.path(getwd(), "functions", "simulation.R"))
source(file.path(getwd(), "functions", "dgp.R"))
source(file.path(getwd(), "functions", "mrot.R"))
source(file.path(getwd(), "functions","rd.R"))

# set seed for reproducibility
set.seed(4322)

#-------------------         fix parameters          ---------------------------

S <- 1000
alpha <- 0.05

#-----        Analysis of coverage probability and interval length          ----

n <- c(500)

# Basis for construction of combinations
data_model <- c(
  "design_1",
  "design_2",
  "design_3",
  "design_4",
  "design_5"
)

mrot_method <- c(
  "true_m",
  "Imbens"
)

kernel <- c("triangular")

# Methods with uniform bandwidth
uniform_params <- tibble::tribble(
  ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
   "honest",      "MSE",               TRUE,      "nn",
   "honest",     "FLCI",               TRUE,      "nn",
      "rbc",      "MSE",               TRUE,      "nn"
) 

uniform_grid <- expand.grid(
  n = n,
  data_model = data_model,
  mrot_method = mrot_method,
  kernel = kernel
)
  

uniform_params_expanded <- dplyr::cross_join(uniform_params, uniform_grid)

# Methods with no uniform bandwidth
not_uniform_params <- tibble::tribble(
  ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
      "rbc",      "MSE",               FALSE,      "nn",
      "rbc",       "CE",               FALSE,      "nn",
)

not_uniform_grid <- expand.grid(
  n = n,
  data_model = data_model,
  mrot_method = NA,
  kernel = kernel
)

not_uniform_params_expanded <- dplyr::cross_join(not_uniform_params, not_uniform_grid)

# Combine Grids

coverage_prob_grid <- rbind(not_uniform_params_expanded, uniform_params_expanded)

grid_length <- nrow(coverage_prob_grid)
  
# Estimate coverage probability for all combinations
for (i in c(1:grid_length)){
  
  param <- coverage_prob_grid[i,]
  
  estimates <- simulation( 
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
  coverage_prob_grid[i,"m_sd"]            <- estimates$m_sd
  
  print(paste0(i," / ", grid_length))
  print(coverage_prob_grid[i,])
  
}

# Analysis of CI Methods

ci_analysis <- coverage_prob_grid %>% dplyr::mutate(
  bw_method_extended = paste0(bw_method, " (ci = ",ci_method,", uni = ",bw_method_uniform,", mrot = ",mrot_method,")") 
)

plotly::plot_ly(
  ci_analysis,
  x = ~data_model, 
  y = ~interval_length, 
  color = ~bw_method_extended, 
  type = 'scatter'
  , mode = 'lines+markers'
) %>% 
  layout(
    yaxis = list(
      title = "Interval length"  
    ),
    xaxis = list(
      title = "DGP"
    )
  )

plotly::plot_ly(
  ci_analysis,
  x = ~data_model, 
  y = ~coverage_prob, 
  color = ~bw_method_extended, 
  type = 'scatter'
  , mode = 'lines+markers'
) %>% 
  layout(
    yaxis = list(
      title = "Coverage Probability",
      tickvals = seq(0.9, 1, by = 0.02)  
    ),
    xaxis = list(
      title = "DGP"
    )
  )

# Analysis of M Methods
analysis <- coverage_prob_grid %>% dplyr::mutate(
  m_hat_norm = 
    case_when(
      mrot_method == "Imbens" ~ 2*(m_hat/2) - m,
      .default = m_hat - m
    )
)

plotly::plot_ly(
  analysis,
  x = ~data_model, 
  y = ~m_hat_norm, 
  color = ~mrot_method, 
  type = 'scatter'
  , mode = 'lines+markers'
) %>% 
  layout(
    yaxis = list(
      title = "m_hat - m",
      tickvals = seq(-4, 4, by = 1)  
    ),
    xaxis = list(
      title = "DGP"
    )
  )
  
plotly::plot_ly(
  analysis,
  x = ~data_model, 
  y = ~m_sd, 
  color = ~mrot_method, 
  type = 'scatter'
  , mode = 'lines+markers'
) %>% 
  layout(
    yaxis = list(
      title = "SD(m_hat)",
      tickvals = seq(0, 4, by = 1)  
    ),
    xaxis = list(
      title = "DGP"
    )
  )

plotly::plot_ly(
  analysis,
  x = ~data_model, 
  y = ~coverage_prob, 
  color = ~mrot_method, 
  type = 'scatter'
  , mode = 'lines+markers'
) %>% 
  layout(
    yaxis = list(
      title = "Interval length"
    ),
    xaxis = list(
      title = "DGP"
    )
  )
