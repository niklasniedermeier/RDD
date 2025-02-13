
# Analysis of CI Methods
plot_compare_ci_methods <- function(grid){
  
  ci_analysis <- grid %>% dplyr::mutate(
    data_model = gsub(".*_(\\d+)$", "\\1", data_model),
    bw_method_extended = paste0(bw_method, " (ci = ",ci_method,", uni = ",bw_method_uniform,", bw = ",bw_method,")") 
  ) %>% dplyr::arrange(
    .data$ci_method
  ) #%>% dplyr::filter(
    #bw_method == "MSE"
  #) 
  
  line = list(width = 1.5) # Set line width here
  marker = list(size = 3)

  y_tickvals_bias <- seq(-0.05, 0.1, 0.05)
  y_range_bias    <-   c(-0.02, 0.12)
  y_title_bias    <- TeX("\\text{Bias}(\\hat{\\tau})")
  
  y_tickvals_se   <- seq( 0.1, 0.25, 0.05)
  y_range_se      <-   c( 0.1, 0.28)
  y_title_se      <- TeX("\\text{SE}(\\hat{\\tau})")
  
  y_tickvals_cp   <- seq(0.6, 1, 0.1)
  y_range_cp      <-   c(0.6, 1)
  y_title_cp      <- TeX("\\text{CP}(\\hat{\\tau})")
  
  y_tickvals_il   <- seq(0.4, 1, 0.2)
  y_range_il      <-   c(0.4, 1.1)
  y_title_il      <- TeX("\\text{IL}(\\hat{\\tau})")
  
  y_tickvals_bw   <- seq(0.3, 0.7, 0.1)
  y_range_bw      <-   c(0.25, 0.7)
  y_title_bw      <- TeX("\\hat{h}")
  
  x_title         <- "DGP"
  
  dotted_line <- list(
    list(
      type = "line",
      x0 = 0,  
      x1 = 4,  
      y0 = 0.95, 
      y1 = 0.95, 
      line = list(
        dash = "dot",   
        color = "grey", 
        width = 2       
      )
    )
  )
  
  annotations = list( 
    list( 
      x = 0.45,  
      y = 1.05,  
      text = "M = 4",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.45,  
      y = 0.5,  
      text = "M = 8",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    )
  )
  
  unique_methods <- ci_analysis %>% 
    dplyr::select(.data$bw_method_extended) %>% 
    dplyr::pull() %>% 
    unique()
  
  plotly_colors <- c(
    "#1f77b4",  # blue
    "#ff7f0e",  # orange
    "#2ca02c",  # green
    "#d62728",  # red
    "#9467bd",  # purple
    "#8c564b",  # brown
    "#e377c2",  # pink
    "#7f7f7f"  # gray
  )  
  plotly_colors <- plotly_colors[1:length(unique_methods)]
  
  setNames(unique_methods, plotly_colors)
  
  ########## M = 4
  
  # Interval length
  
  plot_il_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
    x = ~data_model, 
    y = ~interval_length, 
    color = ~bw_method_extended, 
    colors = plotly_colors,
    marker = marker,
    line = line,
    type = 'scatter', 
    mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_il,
        tickvals = y_tickvals_il,
        range   = y_range_il   
      ),
      xaxis = list(
        title = x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  # Bias
  
  plot_bias_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
    x = ~data_model, 
    y = ~tau_hat-0.25, 
    color = ~bw_method_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_bias,
        tickvals = y_tickvals_bias,
        range   = y_range_bias   
      ),
      xaxis = list(
        title = x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  # SE
  
  plot_se_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
    x = ~data_model, 
    y = ~tau_hat_se, 
    color = ~bw_method_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F

  ) %>% 
    layout(
      yaxis = list(
        title = y_title_se,
        tickvals = y_tickvals_se,
        range   = y_range_se   
      ),
      xaxis = list(
        title =  x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  
  # CP
  
  plot_cp_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~bw_method_extended,
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_cp,
        tickvals = y_tickvals_cp,
        range   = y_range_cp   
      ),
      xaxis = list(
        title =  x_title
      ),
      shapes = dotted_line
    )  %>%
    config(mathjax = "cdn")
  
  # BW 
  
  plot_bw_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
    x = ~data_model, 
    y = ~h_hat, 
    color = ~bw_method_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_bw,
        tickvals = y_tickvals_bw,
        range   = y_range_bw   
      ),
      xaxis = list(
        title =  x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  ######## M = 8
  
  # Interval length
  
  plot_il_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
    x = ~data_model, 
    y = ~interval_length, 
    color = ~bw_method_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter', 
    mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_il,
        tickvals = y_tickvals_il,
        range   = y_range_il   
      ),
      xaxis = list(
        title = x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  # Bias
  
  plot_bias_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
    x = ~data_model, 
    y = ~tau_hat-0.25, 
    color = ~bw_method_extended,
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_bias,
        tickvals = y_tickvals_bias,
        range   = y_range_bias   
      ),
      xaxis = list(
        title = x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  # SE
  
  plot_se_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
    x = ~data_model, 
    y = ~tau_hat_se, 
    color = ~bw_method_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_se,
        tickvals = y_tickvals_se,
        range   = y_range_se   
      ),
      xaxis = list(
        title = x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  
  # CP
  
  plot_cp_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~bw_method_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_cp,
        tickvals = y_tickvals_cp,
        range   = y_range_cp   
      ),
      xaxis = list(
        title = x_title
      ),
      shapes = dotted_line
    ) %>%
    config(mathjax = "cdn")
  
  # BW
  
  plot_bw_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
    x = ~data_model, 
    y = ~h_hat, 
    color = ~bw_method_extended,
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_bw,
        tickvals = y_tickvals_bw,
        range   = y_range_bw   
      ),
      xaxis = list(
        title = x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  plot <- subplot(plot_bias_m_4,  
                  plot_se_m_4,   
                  plot_cp_m_4,    
                  plot_il_m_4,  
              
                  plot_bias_m_8,
                  plot_se_m_8,
                  plot_cp_m_8,
                  plot_il_m_8,
                  
                  nrows = 2,
                  margin = 0.05,
                  titleX = T,
                  titleY = T,
                  shareX = TRUE
  ) %>% 
    layout(title = paste0("M = ",unique(ci_analysis$M)),
           legend = list(x = 0,y = -0.15,
                         orientation = "h",
                         #tracegroupgap = 1,
                         font = list(size = 10)
           ),
           annotations = annotations,
           margin = list(l = 60, r = 40, b = 60, t = 60) 

    ) %>%
    config(mathjax = "cdn")
  
  plot
}


## BW plot
plot_compare_h_methods <- function(grid){
  
  ci_analysis <- grid %>% 
    distinct(M, data_model, bw_method, bw_method_uniform, .keep_all = TRUE) %>%
    dplyr::mutate(
      data_model         = gsub(".*_(\\d+)$", "\\1", data_model),
      bw_method_extended = paste0(bw_method, " (uni = ",bw_method_uniform,")") 
    ) %>% dplyr::arrange(
      .data$ci_method
    ) 
  
  line = list(width = 2) 
  marker = list(size = 4)
  
  y_tickvals_bw   <- seq(0.3, 0.7, 0.1)
  y_range_bw      <-   c(0.25, 0.7)
  y_title_bw      <- TeX("\\hat{h}")
  
  x_title         <- "DGP"
  
  height = 400
  annotations = list( 
    list( 
      x = 0.22,  
      y = 1.1,  
      text = "M = 4",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.77,  
      y = 1.1,  
      text ="M = 8",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    )
  )
  
  unique_methods <- ci_analysis %>% 
    dplyr::select(.data$bw_method_extended) %>% 
    dplyr::pull() %>% 
    unique()
  
  plotly_colors <- c(
    "#7f7f7f",  # gray
    "#9467bd",  # purple
    "#d62728",  # red
    "#2ca02c"  # green
  )  
  plotly_colors <- plotly_colors[1:length(unique_methods)]
  
  setNames(unique_methods, plotly_colors)

  ########## M = 4
  
  plot_bw_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
    x = ~data_model, 
    y = ~h_hat, 
    color = ~bw_method_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      height = height,  
      yaxis = list(
        title = y_title_bw,
        tickvals = y_tickvals_bw,
        range   = y_range_bw   
      ),
      xaxis = list(
        title =  x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  ######## M = 8

  plot_bw_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
    x = ~data_model, 
    y = ~h_hat, 
    color = ~bw_method_extended,
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      height = height,  
      yaxis = list(
        title = y_title_bw,
        tickvals = y_tickvals_bw,
        range   = y_range_bw   
      ),
      xaxis = list(
        title = x_title
      )
    ) %>%
    config(mathjax = "cdn")
  
  plot <- subplot(plot_bw_m_4,    
                  plot_bw_m_8,
                  nrows = 1,
                  margin = 0.05,
                  titleX = T,
                  titleY = T,
                  shareY = TRUE,
                  shareX = TRUE
  ) %>% 
    layout(title = paste0("M = ",unique(ci_analysis$M)),
           legend = list(x = 0,y = -0.15,
                         orientation = "h",
                         #tracegroupgap = 1,
                         font = list(size = 10)
           ),
           annotations = annotations,
           margin = list(l = 60, r = 40, b = 60, t = 60) 
           
    ) %>%
    config(mathjax = "cdn")
  
  plot
}



## MROT Plot
plot_compare_mrot_methods <- function(grid, M, title){
  mrot_analysis <- grid  %>% 
    dplyr::filter(
      .data$M == .env$M
    ) %>% 
    dplyr::mutate(
      data_model = gsub(".*_(\\d+)$", "\\1", data_model),
      mrot_extended = as.character(mrot_method) 
    ) %>% dplyr::arrange(
      mrot_method
    )
  
  line = list(width = 2) # Set line width here
  marker = list(size = 4)
  
  y_tickvals_bias <- seq(-5, 35, 5)
  y_range_bias    <-   c(-6, 35)
  y_title_bias    <- TeX("\\text{Bias}(\\hat{\\text{M}})")
  
  y_tickvals_sd   <- seq( 0, 15, 5)
  y_range_sd      <-   c( 0, 15)
  y_title_sd      <- TeX("\\text{SD}(\\hat{\\text{M}})")
  
  y_tickvals_cp   <- seq(0.6, 1, 0.1)
  y_range_cp      <-   c(0.6, 1)
  y_title_cp      <- TeX("\\text{CP}(\\hat{\\tau})")
  
  y_tickvals_il   <- seq(0.4, 1, 0.2)
  y_range_il      <-   c(0.4, 1.05)
  y_title_il      <- TeX("\\text{IL}(\\hat{\\tau})")
  
  x_title         <- TeX("\\text{DGP}")
  
  
  if (any(grepl("^Kolesar",mrot_analysis$mrot_method))){
    y_tickvals_sd   <- seq( 0, 40, 5)
    y_range_sd      <-   c( 0, 40)
  }
  
  dotted_line <- list(
    list(
      type = "line",
      x0 = 0,  
      x1 = 4,  
      y0 = 0.95, 
      y1 = 0.95, 
      line = list(
        dash = "dot",   
        color = "grey", 
        width = 2       
      )
    )
  )
  
  annotations = list( 
    list( 
      x = 0.5,  
      y = 1.1,  
      text = title,  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    )
  )
  unique_methods <- mrot_analysis %>% select(.data$mrot_extended) %>% pull() %>% unique()

  plotly_colors <- c(
    "#1f77b4",  # blue
    "#ff7f0e",  # orange
    "#2ca02c",  # green
    "#d62728",  # red
    "#9467bd",  # purple
    "#8c564b",  # brown
    "#e377c2",  # pink
    "#7f7f7f",  # gray
    "#bcbd22",  # yellow
    "#17becf"   # cyan
  )  
  plotly_colors = plotly_colors[1:length(unique_methods)]
  
  setNames(unique_methods, plotly_colors)
  
  # Bias

  plot_bias <- plotly::plot_ly(
    mrot_analysis ,
    x = ~data_model, 
    y = ~m_hat-M, 
    color = ~mrot_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_bias,
        tickvals = y_tickvals_bias,
        range   = y_range_bias
      ),
      xaxis = list(
        title =  x_title 
      )
    )%>%
    config(mathjax = "cdn")
  
  # SD
  
  plot_sd <- plotly::plot_ly(
    mrot_analysis ,
    x = ~data_model, 
    y = ~m_sd, 
    color = ~mrot_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
    
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_sd,
        tickvals = y_tickvals_sd,
        range   = y_range_sd 
      ),
      xaxis = list(
        title =  x_title 
      )
    )%>%
    config(mathjax = "cdn")
  
  # CP 
  plot_cp <- plotly::plot_ly(
    mrot_analysis ,
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~mrot_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
    
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_cp,
        tickvals = y_tickvals_cp,
        range   = y_range_cp   
      ),
      xaxis = list(
        title =  x_title 
      ),
      shapes = dotted_line
    )%>%
    config(mathjax = "cdn")
  
  # IL
  plot_il <- plotly::plot_ly(
    mrot_analysis ,
    x = ~data_model, 
    y = ~interval_length, 
    color = ~mrot_extended, 
    colors = plotly_colors,
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
    
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_il,
        tickvals = y_tickvals_il,
        range   = y_range_il  
      ),
      xaxis = list(
        title =  x_title 
      )
    )%>%
    config(mathjax = "cdn")
  
  plot <- subplot(
    plot_bias,
    plot_sd,
    plot_cp,
    plot_il,
    nrows = 1,
    margin = 0.05,
    titleX = T,
    titleY = T,
    shareX = TRUE
  ) %>% 
    layout(
      legend = list(x = 0,y = 1.2,
                         orientation = "h",
                         tracegroupgap = 1,
                         font = list(size = 10)
           ),
      annotations = annotations,
      margin = list(l = 60, r = 60, b = 60, t = 60) 
    ) %>%
    config(mathjax = "cdn")
  
  return(plot)
}



plot_compare_mrot_methods_fix <- function(coverage_prob_grid){
  
  y_tickvals_cp   <- c(seq(0, 1, 0.2),0.95)
  y_range_cp      <-   c(0, 1)
  y_title_cp      <- TeX("\\text{CP}(\\hat{\\tau})")
  
  y_tickvals_il   <- seq(0, 1, 0.2)
  y_range_il      <-   c(0, 1)
  y_title_il      <- TeX("\\text{IL}(\\hat{\\tau})")
  
  y_tickvals_cv   <- seq(1.6, 2.4, 0.2)
  y_range_cv      <-   c(1.6, 2.4)
  y_title_cv      <- TeX("\\text{cv}(\\hat{t})")
  
  y_tickvals_h    <- seq(0.2, 0.6, 0.1)
  y_range_h       <-   c(0.2, 0.6)
  y_title_h       <- TeX("\\hat{h}")
  
  line = list(width = 2.5) # Set line width here
  marker = list(size = 4.5)
  
  height = 500
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
  
  cv <- plotly::plot_ly(
    data,
    x = ~m_hat, 
    y = ~cv, 
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
        title = y_title_cv,
        tickvals = y_tickvals_cv,
        range   = y_range_cv  
      ),
      xaxis = list(
        title = x_title,
        tickvals = x_tickvals,
        range = x_range 
      )
    ) %>%
    config(mathjax = "cdn")
  
  h_hat <- plotly::plot_ly(
    data ,
    x = ~m_hat, 
    y = ~h_hat, 
    color = ~data_model,
    colors = plotly_colors,
    type = 'scatter', 
    mode = 'lines+markers',
    showlegend = F,
    line = line,
    marker = marker
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_h,
        tickvals = y_tickvals_h,
        range   = y_range_h 
      ),
      xaxis = list(
        title =  x_title 
      )
    )%>%
    config(mathjax = "cdn")
  
  subplot(
    #h_hat,
    #cv,
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
      ),
      margin = list(l = 60, r = 40, b = 30, t = 50) 
    ) %>%
    config(mathjax = "cdn")
}


annotations <- list(
  # Global Polynomial Point Estimation
  list(
    x = 0.85, y = 1.045, xref = "paper", yref = "paper",
    text = "<span style='color:#1f77b4'>●</span> q=2",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 0.9, y = 1.045, xref = "paper", yref = "paper",
    text = "<span style='color:#ff7f0e'>●</span> q=2",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 0.95, y = 1.045, xref = "paper", yref = "paper",
    text = "<span style='color:#2ca02c'>●</span> q=3",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 1, y = 1.045, xref = "paper", yref = "paper",
    text = "<span style='color:#d62728'>●</span> q=4",
    showarrow = FALSE, font = list(size = 10)
  ),
  
  # Scaled
  list(
    x = 0.85, y = 0.74, xref = "paper", yref = "paper",
    text = "<span style='color:#1f77b4'>●</span> m=2",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 0.9, y = 0.74, xref = "paper", yref = "paper",
    text = "<span style='color:#ff7f0e'>●</span> m=3",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 0.95, y = 0.74, xref = "paper", yref = "paper",
    text = "<span style='color:#2ca02c'>●</span> m=4",
    showarrow = FALSE, font = list(size = 10)
  ),
  
  # Global Spline Regression
  list(
    x = 0.85, y = 0.48, xref = "paper", yref = "paper",
    text = "<span style='color:#1f77b4'>●</span> k=2",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 0.9, y =  0.48, xref = "paper", yref = "paper",
    text = "<span style='color:#ff7f0e'>●</span> k=3",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 0.95, y =  0.48, xref = "paper", yref = "paper",
    text = "<span style='color:#2ca02c'>●</span> k=4",
    showarrow = FALSE, font = list(size = 10)
  ),
  
  # Half Bias Point Estimate 
  list(
    x = 0.85, y = 0.22, xref = "paper", yref = "paper",
    text = "<span style='color:#1f77b4'>●</span> s=15",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 0.9, y =  0.22, xref = "paper", yref = "paper",
    text = "<span style='color:#ff7f0e'>●</span> s=20",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 0.95, y =  0.22, xref = "paper", yref = "paper",
    text = "<span style='color:#2ca02c'>●</span> s=25",
    showarrow = FALSE, font = list(size = 10)
  ),
  list(
    x = 1, y = 0.22, xref = "paper", yref = "paper",
    text = "<span style='color:#d62728'>●</span> s=30",
    showarrow = FALSE, font = list(size = 10)
  )
)

