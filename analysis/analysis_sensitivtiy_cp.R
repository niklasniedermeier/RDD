# import libs
source(file.path(getwd(), "lib.R"))

# import sources
source(file.path(getwd(), "functions", "simulation.R"))
source(file.path(getwd(), "functions", "dgp.R"))
source(file.path(getwd(), "functions", "mrot.R"))
source(file.path(getwd(), "functions","rd.R"))
source(file.path(getwd(), "functions","plots.R"))

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

m_hat <- c(1,2,4,8,16)
mrot_method <- paste0("M_",m_hat)


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

y_tickvals_cp   <- seq(0, 1, 0.1)
y_range_cp      <-   c(0, 1)
y_title_cp      <- TeX("\\text{CP}(\\hat{\\tau})")

y_tickvals_il   <- seq(0, 1, 0.2)
y_range_il      <-   c(0, 1)
y_title_il      <- TeX("\\text{IL}(\\hat{\\tau})")

line = list(width = 2.5) # Set line width here
marker = list(size = 4.5)

height = 480
dotted_line <- list(
  list(
    type = "line",
    x0 = 1,  
    x1 = 16,  
    y0 = 0.95, 
    y1 = 0.95, 
    line = list(
      dash = "dot",   
      color = "grey", 
      width = 2       
    )
  )
)

plotly_colors <- c(
  "design_1" = "#1f77b4",  # blue
  "design_2" = "#ff7f0e",  # orange
  "design_3" = "#2ca02c",  # green
  "design_4" = "#d62728",  # red
  "design_5" = "#9467bd"  # purple
)

data <- coverage_prob_grid %>% 
  dplyr::filter(mrot_method != "M_0.5") %>% 
  dplyr::arrange(data_model) 

x_title         <- TeX("\\hat{M}")
x_tickvals      <- sort(unique(data$m_hat))
x_range         <- c(min(x_tickvals)-0.1,max(x_tickvals)+0.1)

cp <- plotly::plot_ly(
  data,
  x = ~m_hat, 
  y = ~coverage_prob, 
  color = ~data_model, 
  colors = plotly_colors,
  type = 'scatter', 
  mode = 'lines+markers',
  showlegend = T,
  line = line,
  marker = marker
) %>% 
  layout(
    height = height,  
    yaxis = list(
      title = y_title_cp,
      tickvals = y_tickvals_cp,
      range   = y_range_cp   
    ),
    xaxis = list(
      title = x_title,
      tickvals = x_tickvals,
      range = x_range   
    ),
    shapes = dotted_line
  ) %>%
  config(mathjax = "cdn")

il <- plotly::plot_ly(
  data,
  x = ~m_hat, 
  y = ~interval_length, 
  color = ~data_model,
  colors = plotly_colors,
  type = 'scatter', 
  mode = 'lines+markers',
  showlegend = F,
  line = line,
  marker = marker
) %>% 
  layout(
    height = height,  
    yaxis = list(
      title = y_title_il,
      tickvals = y_tickvals_il,
      range   = y_range_il   
    ),
    xaxis = list(
      title = x_title,
      tickvals = x_tickvals,
      range = x_range 
    )
  ) %>%
  config(mathjax = "cdn")



subplot(
  cp,
  il,
  nrows = 1,
  margin = 0.05,
  titleX = T,
  titleY = T
) %>% 
  layout(
    legend = list(x = 0.65,y = 0.4,
                 orientation = "v",
                 font = list(size = 12)
     )

  ) %>%
  config(mathjax = "cdn")

