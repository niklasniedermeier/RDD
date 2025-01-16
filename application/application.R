# import libs
source(file.path(getwd(), "lib.R"))
source(file.path(getwd(), "functions","rd.R"))
source(file.path(getwd(), "functions", "mrot.R"))

#############################################################
## Load and setup data
#############################################################

path = file.path(getwd(),"data","application","headstart.csv")
data <- read.csv(path)
dim(data)
names(data)

cutoff <- 59.1984
Y <- data$mort_age59_related_postHS
Rraw <- data$povrate60
R <- data$povrate60 - cutoff

D <- as.numeric(R>=0)

# Placebo outcomes
Y_placebo_post <- data$mort_age59_injury_postHS
Y_placebo_pre <- data$mort_age59_related_preHS

#############################################################
## Scatter and RD Plot, Head Start Data
#############################################################
ii <- which(Y<20 & !is.na(Y) & !is.na(Rraw))
rdplot(Y[ii],Rraw[ii],c=59.1984,nbins=3000)
rdplot(Y[ii],Rraw[ii],c=59.1984)

#############################################################
## Robust Nonparametric Local polynomial Methods
#############################################################

cutoff_norm = 0
kernel = "triangular"
se_method = "nn"
se_method_J = 3
alpha = 0.05

params <- tibble::tribble(
      ~ci_method, ~bw_method,  ~bw_method_uniform,  ~mrot_method,
        "honest",      "MSE",                TRUE,    "poly_p_2",
        "honest",      "MSE",                TRUE,    "poly_p_3",
        "honest",      "MSE",                TRUE,    "poly_p_4",
        
        "honest",     "FLCI",                TRUE,    "poly_p_2",
        "honest",     "FLCI",                TRUE,    "poly_p_3",
        "honest",     "FLCI",                TRUE,    "poly_p_4",
      
           "rbc",      "MSE",               FALSE,            NA, 
           "rbc",       "CE",               FALSE,            NA,
  
  "conventional",      "MSE",               FALSE,            NA
)

grid_length <- nrow(params)

rd_estimates <- data.frame()

# Estimate coverage probability for all combinations
for (i in c(1:grid_length)){
  
  param <- params[i,]
  
  if (!is.na(param$mrot_method)){
    if (param$mrot_method == "poly_p_2"){
      m_hat <- mrot(X = R, Y = Y, method = "poly", c = cutoff_norm, p = 2)
    }
    
    if (param$mrot_method == "poly_p_3"){
      m_hat <- mrot(X = R, Y = Y, method = "poly", c = cutoff_norm, p = 3)
    }
    
    if (param$mrot_method == "poly_p_4"){
      m_hat <- mrot(X = R, Y = Y, method = "poly", c = cutoff_norm, p = 4)
    }
  }else{
    m_hat <- NA
  }
  

  
  tmp <- suppressMessages(
    rd(
      Y = Y,
      X = R,
      cutoff = cutoff_norm,
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


#test <- rd_estimates %>%
#  dplyr::mutate(
#    calc = 2 * (1- pnorm(abs(tau_hat / tau_hat_se)))
#    )

