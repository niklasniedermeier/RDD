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


mrot_method <- c("true_m")
M <- c(2)
kernel <- c("triangular")

# Methods with uniform bandwidth
uniform_params <- tibble::tribble(
  ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
   "honest",      "MSE",               TRUE,      "nn",
   "honest",     "FLCI",               TRUE,      "nn",
      "rbc",      "MSE",               TRUE,      "nn"
) 

uniform_grid <- expand.grid(
  n           = n,
  data_model  = data_model,
  mrot_method = mrot_method,
  M           = M,
  kernel      = kernel
)
  

uniform_params_expanded <- dplyr::cross_join(uniform_params, uniform_grid)

# Methods with no uniform bandwidth
not_uniform_params <- tibble::tribble(
     ~ci_method, ~bw_method, ~bw_method_uniform, ~se_method,
          "rbc",      "MSE",               FALSE,      "nn",
          "rbc",       "CE",               FALSE,      "nn",
 "conventional",      "MSE",               FALSE,      "nn"
)

not_uniform_grid <- expand.grid(
  n           = n,
  data_model  = data_model,
  mrot_method = NA,
  M           = M,
  kernel      = kernel
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

ci_analysis <- coverage_prob_grid %>% dplyr::mutate(
  data_model = gsub(".*_(\\d+)$", "\\1", data_model),
  bw_method_extended = paste0(bw_method, " (ci = ",ci_method,", uni = ",bw_method_uniform,", mrot = ",mrot_method,")") 
) %>% arrange(
  .data$ci_method
) #%>% dplyr::filter(
  #.data$M == 8
#)

line = list(width = 1.2) # Set line width here
marker = list(size = 4)
# Interval length

plot_il <- plotly::plot_ly(
  ci_analysis,
  x = ~data_model, 
  y = ~interval_length, 
  color = ~bw_method_extended, 
  line = line,
  marker = marker,
  type = 'scatter', 
  mode = 'lines+markers',
  showlegend = F
) %>% 
  layout(
    yaxis = list(
      title = "IL"  
    ),
    xaxis = list(
      title = "DGP"
    )
  )

# Bias

plot_bias <- plotly::plot_ly(
  ci_analysis,
  x = ~data_model, 
  y = ~tau_hat-0.25, 
  color = ~bw_method_extended, 
  line = line,
  marker = marker,
  type = 'scatter'
  , mode = 'lines+markers',
  showlegend = F
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

plot_se <- plotly::plot_ly(
  ci_analysis,
  x = ~data_model, 
  y = ~tau_hat_se, 
  color = ~bw_method_extended, 
  line = line,
  marker = marker,
  type = 'scatter'
  , mode = 'lines+markers',
  showlegend = T
  
) %>% 
  layout(
    yaxis = list(
      title = "SE"  
    ),
    xaxis = list(
      title = "DGP"
    )
  )


# CP

y_axis_range <- c(0.75,1)
y_axis_tickvals <- seq(0.75,1,0.05)

plot_cp <- plotly::plot_ly(
  ci_analysis,
  x = ~data_model, 
  y = ~coverage_prob, 
  color = ~bw_method_extended, 
  #linetype = ~bw_method_extended,
  line = line,
  marker = marker,
  type = 'scatter'
  , mode = 'lines+markers',
  showlegend = F
) %>% 
  layout(
    yaxis = list(
      title = "CP",
      range = y_axis_range,
      tickvals = y_axis_tickvals
    ),
    xaxis = list(
      title = "DGP"
    )
  )

plot_bw <- plotly::plot_ly(
  ci_analysis,
  x = ~data_model, 
  y = ~h_hat, 
  color = ~bw_method_extended, 
  line = line,
  marker = marker,
  type = 'scatter'
  , mode = 'lines+markers',
  showlegend = F
) %>% 
  layout(
    yaxis = list(
      title = "Bandwidth"
    ),
    xaxis = list(
      title = "DGP"
    )
  )

plot <- subplot(plot_cp,
                plot_il,
                plot_bias,
                plot_se,
                plot_bw,
                nrows = 3,
                margin = 0.09,
                titleX = T,
                titleY = T
) %>% 
  layout(title = paste0("M = ",unique(ci_analysis$M)),
         legend = list(x = 0.5,y = 0,
                       orientation = "v",
                       tracegroupgap = 1,
                       font = list(size = 10)
         )
  )

plot

