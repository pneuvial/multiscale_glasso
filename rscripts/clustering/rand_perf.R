
# Launch simulations ------------------------------------------------------
## Settings ----------------------------------------------------------------
### Model -------------------------------------------------------------------
p         <- 40
seq_n     <- c(20, 40, 80) 
alpha     <- rep(1/5, 5)
seq_rho   <- c(0.25, 0.95)
seq_dnsty <- c(0.75)
type      <- NA     #1 ## unused to do: delete in configs_simu parameters
ngroup    <- length(alpha)
pi        <- diag(0.75, ngroup)

### Simulation --------------------------------------------------------------
n_simu      <- 100
list_ii_rho <- configs_simu(n_simu, seq_rho, seq_dnsty, seq_n, type)
mc_cores    <- min(80, length(list_ii_rho))
RNGkind("L'Ecuyer-CMRG")

## Test --------------------------------------------------------------------
# For a quicker test: 
#   #set nl2 to 2 in one_simu_extended
#   set p = 9 & n = 10
test <- one_simu_extended(list_ii_rho$`1`, verbose = TRUE, model = "block_diagonal")

## Models ------------------------------------------------------------------
# After the quicker test: 
#   reset nl2 to 20

### Stochastic Block Diagonal -----------------------------------------------
runtime_rand50_config_p40_bdiagflip001_allcor <- system.time(
  rand50_config_p40_bdiagflip001_allcor <- mclapply(
    list_ii_rho, 
    FUN = one_simu_extended, 
    model = "block_diagonal",
    mc.cores = mc_cores)
)

save(runtime_rand50_config_p40_bdiagflip001_allcor, 
     file = paste0(path_extended, "runtime_rand100_config_p40_bdiagflip001_allcor.RData"))
save(rand50_config_p40_bdiagflip001_allcor, 
     file = paste0(path_extended, "rand100_config_p40_bdiagflip001_allcor.RData"))

# Handle results ----------------------------------------------------------
# see handle_rand_performances.Rmd
