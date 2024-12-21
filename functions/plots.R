
# Analysis of CI Methods
plot_compare_ci_mse_methods <- function(grid){
  
  ci_analysis <- coverage_prob_grid %>% dplyr::mutate(
    data_model = gsub(".*_(\\d+)$", "\\1", data_model),
    bw_method_extended = paste0(bw_method, " (ci = ",ci_method,", uni = ",bw_method_uniform,", bw = ",bw_method,")") 
  ) %>% dplyr::arrange(
    .data$ci_method
  ) %>% dplyr::filter(
    bw_method == "MSE"
  ) 
  
  line = list(width = 2) # Set line width here
  marker = list(size = 4)

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
      x = 0.25,  
      y = 1,  
      text = TeX("\\text{M = 4}"),  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.75,  
      y = 1,  
      text = TeX("\\text{M = 8}"),  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    )
  )
    
  ########## M = 4
  
  # Interval length
  
  plot_il_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
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
        title = y_title_il,
        tickvals = y_tickvals_il,
        range   = y_range_il   
      ),
      xaxis = list(
        title = "DGP"
      )
    )
  
  # Bias
  
  plot_bias_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
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
        title = y_title_bias,
        tickvals = y_tickvals_bias,
        range   = y_range_bias   
      ),
      xaxis = list(
        title = "DGP"
      )
    )
  
  # SE
  
  plot_se_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
    x = ~data_model, 
    y = ~tau_hat_se, 
    color = ~bw_method_extended, 
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
        title = "DGP"
      )
    )
  
  
  # CP
  
  plot_cp_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~bw_method_extended, 
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
        title = "DGP"
      ),
      shapes = dotted_line
    )
  
  # BW 
  
  plot_bw_m_4 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==4),
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
        title = y_title_bw,
        tickvals = y_tickvals_bw,
        range   = y_range_bw   
      ),
      xaxis = list(
        title = "DGP"
      )
    )
  
  ######## M = 8
  
  # Interval length
  
  plot_il_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
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
        title = y_title_il,
        tickvals = y_tickvals_il,
        range   = y_range_il   
      ),
      xaxis = list(
        title = "DGP"
      )
    )
  
  # Bias
  
  plot_bias_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
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
        title = y_title_bias,
        tickvals = y_tickvals_bias,
        range   = y_range_bias   
      ),
      xaxis = list(
        title = "DGP"
      )
    )
  
  # SE
  
  plot_se_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
    x = ~data_model, 
    y = ~tau_hat_se, 
    color = ~bw_method_extended, 
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
        title = "DGP"
      )
    )
  
  
  # CP
  
  plot_cp_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~bw_method_extended, 
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
        title = "DGP"
      ),
      shapes = dotted_line
    )
  
  # BW
  
  plot_bw_m_8 <- plotly::plot_ly(
    ci_analysis %>% dplyr::filter(M==8),
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
        title = y_title_bw,
        tickvals = y_tickvals_bw,
        range   = y_range_bw   
      ),
      xaxis = list(
        title = "DGP"
      )
    )
  
  plot <- subplot(plot_bw_m_4,    plot_bw_m_8,
                  plot_bias_m_4,  plot_bias_m_8,
                  plot_se_m_4,    plot_se_m_8,
                  plot_cp_m_4,    plot_cp_m_8,
                  plot_il_m_4,    plot_il_m_8,
                  
                  nrows = 5,
                  margin = 0.02,
                  titleX = T,
                  titleY = T,
                  shareX = TRUE,
                  shareY = TRUE
  ) %>% 
    layout(title = paste0("M = ",unique(ci_analysis$M)),
           legend = list(x = 0,y = -0.15,
                         orientation = "h",
                         #tracegroupgap = 1,
                         font = list(size = 10)
           ),
           annotations = annotations

    ) %>%
    config(mathjax = "cdn")
  
  plot
}



plot_compare_mrot_methods <- function(grid){
  mrot_analysis <- grid %>% 
    dplyr::mutate(
      data_model = gsub(".*_(\\d+)$", "\\1", data_model),
      mrot_extended = paste0("mrot = ",mrot_method) 
    ) %>% arrange(
      M,
      mrot_extended
    )
  
  line = list(width = 2) # Set line width here
  marker = list(size = 4)
  
  y_tickvals_bias <- seq(-5, 25, 5)
  y_range_bias    <-   c(-5, 25)
  y_title_bias    <- TeX("\\text{Bias}(\\hat{\\text{M}})")
  
  y_tickvals_sd   <- seq( 0, 15, 5)
  y_range_sd      <-   c( 0, 15)
  y_title_sd      <- TeX("\\text{SD}(\\hat{\\text{M}})")
  
  y_tickvals_cp   <- seq(0.6, 1, 0.1)
  y_range_cp      <-   c(0.6, 1)
  y_title_cp      <- TeX("\\text{CP}(\\hat{\\tau})")
  
  y_tickvals_il   <- seq(0.4, 1, 0.2)
  y_range_il      <-   c(0.4, 1)
  y_title_il      <- TeX("\\text{IL}(\\hat{\\tau})")
  
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
      y = 1,  
      text = TeX("\\text{M = 2}"),  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.45,  
      y = 0.75,  
      text = TeX("\\text{M = 4}"),  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.45,  
      y = 0.5,  
      text = TeX("\\text{M = 6}"),  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),
    list( 
      x = 0.45,  
      y = 0.25,  
      text = TeX("\\text{M = 8}"),  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    )
  )
  
  ######### M = 2
  # Bias
  
  plot_bias_m_2 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 2),
    x = ~data_model, 
    y = ~m_hat-M, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    ) 
  
  # SD
  
  plot_sd_m_2 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 2),
    x = ~data_model, 
    y = ~m_sd, 
    color = ~mrot_extended, 
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
    
  ) %>% 
    layout(
      yaxis = list(
        title = y_title_sd,
        title = y_title_sd,
        tickvals = y_tickvals_sd,
        range   = y_range_sd 
      ),
      xaxis = list(
        title = "DGP"
      )
    )
  
  plot_cp_m_2 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 2),
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~mrot_extended, 
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
        title = "DGP"
      ),
      shapes = dotted_line
    )
  
  # IL
  plot_il_m_2 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 2),
    x = ~data_model, 
    y = ~interval_length, 
    color = ~mrot_extended, 
    line = line,
    marker = marker,
    type = 'scatter'
    , mode = 'lines+markers',
    showlegend = F
  )  %>% 
    layout(
      yaxis = list(
        title = y_title_il,
        tickvals = y_tickvals_il,
        range   = y_range_il  
      ),
      xaxis = list(
        title = "DGP"
      )
    )
  
  ######### M = 4
  # Bias
  
  plot_bias_m_4 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 4),
    x = ~data_model, 
    y = ~m_hat-M, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  
  # SD
  
  plot_sd_m_4 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 4),
    x = ~data_model, 
    y = ~m_sd, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  
  # CP 
  plot_cp_m_4 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 4),
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~mrot_extended, 
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
        title = "DGP"
      ),
      shapes = dotted_line
    )
  
  # IL
  plot_il_m_4 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 4),
    x = ~data_model, 
    y = ~interval_length, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  
  ######### M = 6
  # Bias
  
  plot_bias_m_6 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 6),
    x = ~data_model, 
    y = ~m_hat-M, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  
  # SD
  
  plot_sd_m_6 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 6),
    x = ~data_model, 
    y = ~m_sd, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  
  
  # CP 
  plot_cp_m_6 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 6),
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~mrot_extended, 
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
        title = "DGP"
      ),
      shapes = dotted_line
    )
  
  # IL
  plot_il_m_6 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 6),
    x = ~data_model, 
    y = ~interval_length, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  ######### M = 8
  # Bias
  
  plot_bias_m_8 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 8),
    x = ~data_model, 
    y = ~m_hat-M, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  
  # SD
  
  plot_sd_m_8 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 8),
    x = ~data_model, 
    y = ~m_sd, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  
  
  # CP 
  plot_cp_m_8 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 8),
    x = ~data_model, 
    y = ~coverage_prob, 
    color = ~mrot_extended, 
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
        title = "DGP"
      ),
      shapes = dotted_line
    )
  
  # IL
  plot_il_m_8 <- plotly::plot_ly(
    mrot_analysis %>% dplyr::filter(M == 8),
    x = ~data_model, 
    y = ~interval_length, 
    color = ~mrot_extended, 
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
        title = "DGP"
      )
    )
  
  plot <- subplot(
    plot_bias_m_2,
    plot_sd_m_2,
    plot_cp_m_2,
    plot_il_m_2,
    plot_bias_m_4,
    plot_sd_m_4,
    plot_cp_m_4,
    plot_il_m_4,
    plot_bias_m_6,
    plot_sd_m_6,
    plot_cp_m_6,
    plot_il_m_6,
    plot_bias_m_8,
    plot_sd_m_8,
    plot_cp_m_8,
    plot_il_m_8,
    nrows = 4,
    margin = 0.05,
    titleX = T,
    titleY = T,
    shareX = TRUE,
    heights = rep(0.25,4)
  ) %>% 
    layout(title = paste0("M = ",unique(mrot_analysis$M)),
           legend = list(x = 0,y = 1.1,
                         orientation = "h",
                         tracegroupgap = 1,
                         font = list(size = 10)
           ),
           annotations = annotations
    ) %>%
    config(mathjax = "cdn")
  
  return(plot)
}


