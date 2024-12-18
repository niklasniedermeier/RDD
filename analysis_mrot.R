# import libs
source(file.path(getwd(), "lib.R"))

# import sources
source(file.path(getwd(), "functions", "simulation.R"))
source(file.path(getwd(), "functions", "dgp.R"))
source(file.path(getwd(), "functions", "mrot.R"))
source(file.path(getwd(), "functions","rd.R"))

# set seed for reproducibility
set.seed(5321)

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
  #"spline_p_2_k_2",
  "spline_p_2_k_3",
  #"spline_p_3_k_2",
  #"spline_p_3_k_3",
  "Imbens_scale_2",
  #"Imbens_scale_3",
  #"Imbens_scale_4",
  "poly_p_2",
  #"poly_p_3",
  #"poly_p_4",
  "poly_p_2_se"
)
M <- c(8)
kernel <- c("triangular")

# Methods with uniform bandwidth
uniform_params <- tibble::tribble(
  ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
  "honest",      "MSE",               TRUE,      "nn"
) 

uniform_grid <- expand.grid(
  n           = n,
  data_model  = data_model,
  mrot_method = mrot_method,
  M           = M,
  kernel      = kernel
)

coverage_prob_grid <- dplyr::cross_join(uniform_params, uniform_grid)

grid_length <- nrow(coverage_prob_grid)

# Estimate coverage probability for all combinations
for (i in c(1:grid_length)){
  
  param <- coverage_prob_grid[i,]
  
  estimates <- simulation( 
    S                 = S,
    data_model        = as.character(param$data_model),
    n                 = as.integer(param$n),
    mrot_method       = as.character(param$mrot_method),
    M                 = as.integer(param$M),
    kernel            = as.character(param$kernel),
    ci_method         = as.character(param$ci_method),
    bw_method         = as.character(param$bw_method),
    bw_method_uniform = as.logical(param$bw_method_uniform),
    se_method         = as.character(param$se_method),
    alpha             = alpha
  )
  
  coverage_prob_grid[i,"coverage_prob"]   <- estimates$coverage_prob
  coverage_prob_grid[i,"interval_length"] <- estimates$interval_length
  coverage_prob_grid[i,"tau_hat_se"]      <- estimates$tau_hat_se
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

mrot_analysis <- coverage_prob_grid %>% 
  dplyr::mutate(
  data_model = gsub(".*_(\\d+)$", "\\1", data_model),
  bw_method_extended = paste0("mrot = ",mrot_method) 
) %>% arrange(
  .data$ci_method
) 

line = list(width = 1.2) # Set line width here
marker = list(size = 4)

# Bias

plot_bias <- plotly::plot_ly(
  mrot_analysis,
  x = ~data_model, 
  y = ~m_hat-M, 
  color = ~bw_method_extended, 
  line = line,
  marker = marker,
  type = 'scatter'
  , mode = 'lines+markers',
  showlegend = T
) %>% 
  layout(
    yaxis = list(
      title = "Bias"  
    ),
    xaxis = list(
      title = "DGP"
    )
  )

# SE

plot_sd <- plotly::plot_ly(
  mrot_analysis,
  x = ~data_model, 
  y = ~m_sd, 
  color = ~bw_method_extended, 
  line = line,
  marker = marker,
  type = 'scatter'
  , mode = 'lines+markers',
  showlegend = F
  
) %>% 
  layout(
    yaxis = list(
      title = "SD"  
    ),
    xaxis = list(
      title = "DGP"
    )
  )


plot <- subplot(
  plot_bias,
  plot_sd,
  nrows = 2,
  margin = 0.09,
  titleX = T,
  titleY = T
) %>% 
  layout(title = paste0("M = ",unique(mrot_analysis$M)),
         legend = list(x = 0,y = 0,
                       orientation = "h",
                       tracegroupgap = 1,
                       font = list(size = 10)
         )
  )

plot

