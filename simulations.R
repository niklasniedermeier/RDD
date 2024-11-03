source(file.path(getwd(),"functions" ,"coverage_prob.R"))
source(file.path(getwd(),"functions" ,"dgp.R"))
source(file.path(getwd(),"functions" ,"mrot.R"))
source(file.path(getwd(),"functions" ,"rd.R"))

coverage_prob( 
  S = 1000,
  data_model = "m1",
  data_model_m = 5,
  n = 100,
  mrot = "poly_update",
  kernel = "triangular",
  ci_method = "honest",
  bw_method = "MSE",
  bw_method_uniform = TRUE,
  se_method = "nn",
  alpha = 0.05
)

