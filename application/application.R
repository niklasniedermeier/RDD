# import libs
source(file.path(getwd(), "lib.R"))
library(haven)

# import sources
source(file.path(getwd(), "functions", "mrot.R"))
source(file.path(getwd(), "functions","rd.R"))

#Load dataset
path <- file.path(getwd(),"data","data.dta")
data_raw <- haven::read_dta(path)

data <- data_raw %>%
  filter(female == 1, age50 == 1) %>%
  select(unemployment_duration, db) %>%
  arrange(db)

# Variables
cutoff <- -2.5
X <- data %>% select("db") %>% pull()
Y <- data %>% select("unemployment_duration") %>% pull()
alpha <- 0.95

# MROT
m_hat <- mrot(X = X, Y = Y, method = "poly", c = cutoff, p = 2)

#  Honest
rd_estimates <-
  rd(
    Y = X,
    X = X,
    cutoff = cutoff,
    M = 0.05,
    kernel = "triangular",
    ci_method = "honest",
    bw_method = "MSE",
    bw_method_uniform = TRUE,
    se_method = "nn",
    alpha = alpha
  )


