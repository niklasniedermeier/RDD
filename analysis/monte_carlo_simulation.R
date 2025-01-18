# import libs
source(file.path(getwd(), "lib.R"))

# import sources
source(file.path(getwd(), "functions","simulation","simulation.R"))
source(file.path(getwd(), "functions", "dgp.R"))
source(file.path(getwd(), "functions", "mrot.R"))
source(file.path(getwd(), "functions","rd.R"))
source(file.path(getwd(), "functions","plots.R"))

# set seed for reproducibility
set.seed(5321)

# Run and save Monte-Carlo Simulations in data/simulation_results
sim_inference_known_m()
sim_inference_unknown_m_fix()
sim_inference_unknown_m_est()
sim_hetero_error_variance()

