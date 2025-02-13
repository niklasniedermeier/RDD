#-------------------------------------------------------------------------------
#                               Libraries
#-------------------------------------------------------------------------------
source(file.path(getwd(), "lib.R"))

# import sources
source(file.path(getwd(), "functions","simulation","simulation.R"))
source(file.path(getwd(), "functions", "dgp.R"))
source(file.path(getwd(), "functions", "mrot.R"))
source(file.path(getwd(), "functions","rd.R"))
source(file.path(getwd(), "functions","plots.R"))

#-------------------------------------------------------------------------------
#               Run ans save Monte-Carlo-Simulation results
#-------------------------------------------------------------------------------

# set seed for reproducibility
#set.seed(5321)

# Run and save Monte-Carlo Simulations in data/simulation_results
#sim_inference_known_m()
#sim_inference_unknown_m_fix()
#sim_inference_unknown_m_est()
#sim_hetero_error_variance()

#-------------------------------------------------------------------------------
#                 Chapter 5.1: Inference for with known M
#-------------------------------------------------------------------------------

figure_1 <- show_dgp()

path <- file.path(getwd(),"data","simulation_results","sim_inference_known_m.rds")
sim_inference_known_m <- readRDS(path)
  
figure_2 <- plot_compare_h_methods(sim_inference_known_m)
figure_3 <- plot_compare_ci_methods(sim_inference_known_m)

table_5 <- sim_inference_known_m %>% arrange(M,data_model,bw_method_uniform, ci_method) %>%
  mutate(
    Bias = round(tau_hat - 0.25,2),
    SE = round(tau_hat_se,2),
    h = round(h_hat,2),
    CP = round(coverage_prob*100,2),
    IL = round(interval_length,2)
  ) %>%
  select(M, data_model, bw_method_uniform, ci_method, bw_method, Bias, SE, h, CP, IL) 

#-------------------------------------------------------------------------------
#                 # Chapter 5.2: Inference for with unknown M
#-------------------------------------------------------------------------------

path <- file.path(getwd(),"data","simulation_results","sim_inference_unknown_m_fix.rds")
sim_inference_unknown_m_fix <- readRDS(path)

figure_4 <- plot_compare_mrot_methods_fix(sim_inference_unknown_m_fix)

path <- file.path(getwd(),"data","simulation_results","sim_inference_unknown_m_est.rds")
sim_inference_unknown_m_est <- readRDS(path)

poly_results <- sim_inference_unknown_m_est %>% dplyr::filter(grepl("^poly",.data$mrot_method))
imbens_results <- sim_inference_unknown_m_est %>% dplyr::filter(grepl("^Imbens",.data$mrot_method))
spline_results <- sim_inference_unknown_m_est %>% dplyr::filter(grepl("^spline",.data$mrot_method))
hb_results <- sim_inference_unknown_m_est %>% dplyr::filter(grepl("^Kolesar",.data$mrot_method))

figure_5 <- subplot(
  plot_compare_mrot_methods(poly_results, M = 4, "Global Polynomial Regression (Armstrong and Kolesár, 2020)"),
  plot_compare_mrot_methods(imbens_results, M = 4, "Scaled Global Polynomial Regression (Imbens and Wager, 2019)"),
  plot_compare_mrot_methods(spline_results, M = 4, "Global Spline Regression"),
  plot_compare_mrot_methods(hb_results, M = 4, "Half Biased Point Estimate (Kolesár and Rothe, 2018)"),
  nrows = 4,
  margin = 0.05,
  titleX = F,
  titleY = T
) %>%
  config(mathjax = "cdn") %>% layout(title = "M = 4", margin = c(t = 100), annotations = annotations)

figure_6 <- subplot(
  plot_compare_mrot_methods(poly_results, M = 8, "Global Polynomial Regression (Armstrong and Kolesár, 2020)"),
  plot_compare_mrot_methods(imbens_results, M = 8, "Scaled Global Polynomial Regression (Imbens and Wager, 2019)"),
  plot_compare_mrot_methods(spline_results, M = 8, "Global Spline Regression"),
  plot_compare_mrot_methods(hb_results, M = 8, "Half Biased Point Estimate (Kolesár and Rothe, 2018)"),
  nrows = 4,
  margin = 0.05,
  titleX = F,
  titleY = T
) %>%
  config(mathjax = "cdn") %>% layout(title = "M = 8", margin = c(t = 100), annotations = annotations)

table_2  <- sim_inference_unknown_m_est %>% 
  dplyr::group_by(ci_method, mrot_method, M
  ) %>%
  dplyr::summarize(
    cp = paste0(round(min(.data$coverage_prob)*100),"% - ",round(max(.data$coverage_prob)*100),"%"),
    il = paste0(round(min(.data$interval_length),2)," - ",round(max(.data$interval_length),2)),
    il_mean = round(mean(interval_length),2)
  )  %>% 
  ungroup() %>% 
  group_by(ci_method,M) %>%
  mutate(
    diff_il_true = ifelse(
      M==4,
      paste0(round((il_mean/0.65)-1,2)*100,"%"),
      paste0(round((il_mean/0.75)-1,2)*100,"%")
    )
  ) %>% 
  ungroup() 


#-------------------------------------------------------------------------------
#                 # Chapter 5.3: Heteroscedastic Error Variance
#-------------------------------------------------------------------------------

path <- file.path(getwd(),"data","simulation_results","sim_hetero_error_variance.rds")
sim_hetero_error_variance <- readRDS(path)

figure_9 <- show_noise_var() 

table_3 <- sim_hetero_error_variance %>%
  group_by(ci_method, noise_method,mrot_method,se_method_J) %>%
  summarize(
    m_hat = paste0(min(round( m_hat))," - ", max(round(m_hat))),
    cp = paste0(min(round(coverage_prob*100)),"% - ", max(round(coverage_prob*100)),"%"),
    il = mean(interval_length)
  ) %>%
  ungroup() %>%
  group_by(noise_method, se_method_J) %>%
  mutate(
    div_helper = (ci_method == "honest" & mrot_method == "true_m") * il,
    div = max(div_helper),
    il = round(il / div,2)
  ) %>%
  ungroup() %>%
  select(noise_method,ci_method, mrot_method, m_hat, cp, il, se_method_J) %>%
  arrange(noise_method, se_method_J, ci_method) %>% 
  filter(se_method_J == 3)

table_6 <- sim_hetero_error_variance %>%
  group_by(ci_method, noise_method,mrot_method,se_method_J) %>%
  summarize(
    m_hat = paste0(min(round( m_hat))," - ", max(round(m_hat))),
    cp = paste0(min(round(coverage_prob*100)),"% - ", max(round(coverage_prob*100)),"%"),
    il = mean(interval_length)
  ) %>%
  ungroup() %>%
  group_by(noise_method, se_method_J) %>%
  mutate(
    div_helper = (ci_method == "honest" & mrot_method == "true_m") * il,
    div = max(div_helper),
    il = round(il / div,2)
  ) %>%
  ungroup() %>%
  select(noise_method,ci_method, mrot_method, m_hat, cp, il, se_method_J) %>%
  arrange(noise_method, se_method_J, ci_method) %>% 
  filter(se_method_J == 6)

#-------------------------------------------------------------------------------
#                 Chapter 6.1: Data
#-------------------------------------------------------------------------------

## Load and setup data
path = file.path(getwd(),"data","application","headstart.csv")
data <- read.csv(path)
cutoff <- 59.1984
Y <- data$mort_age59_related_postHS
Rraw <- data$povrate60

# Binned Plot
ii <- which(!is.na(Y) & !is.na(Rraw))
binned_plot <- rdplot(Y[ii],Rraw[ii],c=cutoff, p = 4, binselect = "esmv")

X_bin <- binned_plot$vars_bins$rdplot_mean_x
Y_bin <- binned_plot$vars_bins$rdplot_mean_y

X_bin_poly <- binned_plot$vars_poly$rdplot_x
Y_bin_poly <- binned_plot$vars_poly$rdplot_y

figure_7 <- plot_ly() %>%
  add_trace(
    x = ~X_bin, 
    y = ~Y_bin, 
    type = "scatter", 
    mode = "markers", 
    name = "Data Points", 
    marker = list(color = "grey")
    
  ) %>%
  add_trace(
    x = ~X_bin_poly[X_bin_poly < cutoff], 
    y = ~Y_bin_poly[X_bin_poly < cutoff],
    type = "scatter", 
    mode = "lines", 
    name = "Fitted Line", 
    line = list(color = "black")
  ) %>%
  add_trace(
    x = ~X_bin_poly[X_bin_poly > cutoff], 
    y = ~Y_bin_poly[X_bin_poly > cutoff],
    type = "scatter", 
    mode = "lines", 
    name = "Fitted Line", 
    line = list(color = "black")
  ) %>%
  layout(
    xaxis = list(title = "Poverty Rate", titlefont = list(size = 16), tickfont = list(size = 14)),
    yaxis = list(title = "Mortality Rate", titlefont = list(size = 16), tickfont = list(size = 14)),
    showlegend = FALSE,
    shapes = list(
      list(
        type = "line",
        x0 = cutoff, x1 = cutoff, # Position of the vertical line
        y0 = min(Y_bin), y1 = max(Y_bin*1.05), # Span vertically across the data
        line = list(
          color = "black",
          dash = "dash", # Dashed line style
          width = 0.8
        )
      )
    )
  )

#-------------------------------------------------------------------------------
#                 Chapter 6.2: Application of MC-findings
#-------------------------------------------------------------------------------

cutoffs <- c(59.1984)
kernel = "triangular"
se_method = "nn"
se_method_J = 3
alpha = 0.05

params <- tibble::tribble(
  ~ci_method, ~bw_method,  ~bw_method_uniform,        ~mrot_method,
  "honest",      "MSE",                TRUE,    "Imbens_scale_4",
  "honest",      "MSE",                TRUE,    "spline_p_2_k_4",
  
  "rbc",      "MSE",               FALSE,                  NA, 
  
  "conventional",      "MSE",               FALSE,                  NA
)

params <- dplyr::cross_join(data.frame(cutoff = cutoffs),params)

grid_length <- nrow(params)

rd_estimates <- data.frame()

# Estimate coverage probability for all combinations
for (i in c(1:grid_length)){
  
  param <- params[i,]
  
  if (!is.na(param$mrot_method)){
    
    if (param$mrot_method == "Imbens_scale_4"){
      m_hat <- 4 * mrot(X = Rraw, Y = Y, method = "poly", c = cutoff, p = 2)
    }
    
    if (param$mrot_method == "spline_p_2_k_4"){
      m_hat <- mrot(X = Rraw, Y = Y, method = "spline", c = param$cutoff, p = 2, nknots = 4)
    }
  }else{
    m_hat <- NA
  }
  
  
  
  tmp <- suppressMessages(
    rd(
      Y = Y,
      X = Rraw,
      cutoff = param$cutoff,
      M = m_hat,
      kernel = kernel,
      ci_method = param$ci_method,
      bw_method = param$bw_method,
      bw_method_uniform = param$bw_method_uniform,
      se_method = se_method,
      se_method_J = se_method_J,
      alpha = alpha
    )
  )
  
  tmp$mrot_method <- param$mrot_method 
  tmp$m_hat       <- m_hat 
  
  
  rd_estimates <- rbind(rd_estimates, tmp)
  rm(m_hat)
}


table_4 <- rd_estimates %>%
  dplyr::mutate(
    il = abs(conf.high-conf.low)
  )

#-------------------------------------------------------------------------------
#                 Chapter 6.2: Falsification Tests
#-------------------------------------------------------------------------------

cutoffs <- c(30,35,40,45,50,55,65,70,75)
kernel = "triangular"
se_method = "nn"
se_method_J = 3
alpha = 0.05

params <- tibble::tribble(
  ~ci_method, ~bw_method,  ~bw_method_uniform,        ~mrot_method,
  "honest",      "MSE",                TRUE,      "spline_p_2_k_4",
  "rbc",      "MSE",               FALSE,                    NA, 
)

params <- dplyr::cross_join(data.frame(cutoff = cutoffs),params)

grid_length <- nrow(params)

rd_estimates <- data.frame()

# Estimate coverage probability for all combinations
for (i in c(1:grid_length)){
  
  param <- params[i,]
  
  if (!is.na(param$mrot_method)){
    if (param$mrot_method == "spline_p_2_k_2"){
      m_hat <- mrot(X = Rraw, Y = Y, method = "spline", c =  param$cutoff, p = 2, nknots = 2)
    }
    
    if (param$mrot_method == "spline_p_2_k_3"){
      m_hat <- mrot(X = Rraw, Y = Y, method = "spline", c = param$cutoff, p = 2, nknots = 3)
    }
    
    if (param$mrot_method == "spline_p_2_k_4"){
      m_hat <- mrot(X = Rraw, Y = Y, method = "spline", c = param$cutoff, p = 2, nknots = 4)
    }
  }else{
    m_hat <- NA
  }
  
  tmp <- suppressMessages(
    rd(
      Y = Y,
      X = Rraw,
      cutoff = param$cutoff,
      M = m_hat,
      kernel = kernel,
      ci_method = param$ci_method,
      bw_method = param$bw_method,
      bw_method_uniform = param$bw_method_uniform,
      se_method = se_method,
      se_method_J = se_method_J,
      alpha = alpha
    )
  )
  
  tmp$mrot_method <- param$mrot_method 
  tmp$m_hat       <- m_hat 
  
  
  rd_estimates <- rbind(rd_estimates, tmp)
  rm(m_hat)
}


plotly_add_ci <- function(plotly_object, x, y, y_low, y_high, color_1, color_2){
  plotly_object %>%
    add_trace(
      x = x,
      y = y,
      type = 'scatter',
      mode = 'lines',
      line = list(color = color_1, width = 2)
    ) %>%
    # Construction of CIs
    add_trace(
      x = c(x,rev(x)),
      y = c(y_low, rev(y_high)),
      type = 'scatter',
      mode = 'lines',
      line = list(width = 0),
      fillcolor = color_2, 
      fill = "toself"
    )
}

is_rbc_before_c <- rd_estimates$ci_method == "rbc" & rd_estimates$cutoff < 59.1984 
is_rbc_after_c <- rd_estimates$ci_method == "rbc" & rd_estimates$cutoff  > 59.1984 
is_honest_before_c <- rd_estimates$ci_method == "honest" & rd_estimates$cutoff < 59.1984 
is_honest_after_c <- rd_estimates$ci_method == "honest" & rd_estimates$cutoff  > 59.1984 #

figure_8 <- plot_ly() %>%
  plotly_add_ci(
    x = rd_estimates$cutoff[is_rbc_before_c],
    y = rd_estimates$tau_hat[is_rbc_before_c],
    y_low = rd_estimates$conf.low[is_rbc_before_c],
    y_high = rd_estimates$conf.high[is_rbc_before_c],
    color_1 = "#1f77b4",
    color_2 =  "rgba(65, 105, 225, 0.3)"
  ) %>%
  plotly_add_ci(
    x = rd_estimates$cutoff[is_rbc_after_c],
    y = rd_estimates$tau_hat[is_rbc_after_c],
    y_low = rd_estimates$conf.low[is_rbc_after_c],
    y_high = rd_estimates$conf.high[is_rbc_after_c],
    color_1 = "#1f77b4",
    color_2 =  "rgba(65, 105, 225, 0.3)"
  )  %>%
  plotly_add_ci(
    x = rd_estimates$cutoff[is_honest_before_c],
    y = rd_estimates$tau_hat[is_honest_before_c],
    y_low = rd_estimates$conf.low[is_honest_before_c],
    y_high = rd_estimates$conf.high[is_honest_before_c],
    color_1 =  "#ff7f0e",
    color_2 =  "rgba(178, 34, 34, 0.2)"
  ) %>%
  plotly_add_ci(
    x = rd_estimates$cutoff[is_honest_after_c],
    y = rd_estimates$tau_hat[is_honest_after_c],
    y_low = rd_estimates$conf.low[is_honest_after_c],
    y_high = rd_estimates$conf.high[is_honest_after_c],
    color_1 =  "#ff7f0e",
    color_2 =  "rgba(178, 34, 34, 0.2)"
  ) %>%
  layout(
    showlegend = F,
    xaxis = list(title = "Poverty Rate", titlefont = list(size = 16), tickfont = list(size = 14)),
    yaxis = list(title = TeX("\\Large \\mathbf{\\hat{\\tau}}"), titlefont = list(size = 16), tickfont = list(size = 14)),
    shapes = list(
      list(
        type = "line",
        x0 = cutoff, x1 = cutoff, # Position of the vertical line
        y0 = -25, y1 = 25, # Span vertically across the data
        line = list(
          color = "black",
          dash = "dash", # Dashed line style
          width = 0.8
        )
      )
    )
  ) %>%
  config(mathjax = "cdn")


#-------------------------------------------------------------------------------
#                 # Figures
#-------------------------------------------------------------------------------

figure_1
figure_2
figure_3
figure_4
figure_5
figure_6
figure_7
figure_8
figure_9

#-------------------------------------------------------------------------------
#                 # Tables
#-------------------------------------------------------------------------------

table_2
table_3
table_4
table_5
table_6
