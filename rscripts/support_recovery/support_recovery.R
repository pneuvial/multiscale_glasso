
# Performances calculation ------------------------------------------------
# Launched on a cluster using 72 cores

## Settings ----------------------------------------------------------------
### Model -------------------------------------------------------------------
# NA values for some parameter mean they are not relevant
p         <- 40
seq_n     <- c(20, 40, 80)
seq_rho   <- 0.95
seq_dnsty <- NA 
type      <- NA
alpha     <- rep(1/5, 5)
ngroup    <- length(alpha)
pi        <- diag(0.75, ngroup)

### Simulation --------------------------------------------------------------
n_simu      <- 50
list_ii_rho <- configs_simu(n_simu, seq_rho, seq_dnsty, seq_n, type)
no_cores    <- min(72, length(list_ii_rho))

## Test --------------------------------------------------------------------
# For a quicker test: 
#   set nlam1 = 2 & nlam2 = 2 in one_simu_ROC 
#   set p = 9 & n = 10
temp <- one_simu_ROC(list_ii_rho$`1`, verbose = TRUE, model = "block_diagonal")
temp <- one_simu_ROC(list_ii_rho$`1`, verbose = TRUE, model = "erdos")
temp <- one_simu_ROC(list_ii_rho$`1`, verbose = TRUE, model = "scale_free")

## Models ------------------------------------------------------------------
# After the quicker test: 
#   reset nlam1 = 10 & nlam2 = 4 in one_simu_ROC 
### Stochastic Block Diagonal -----------------------------------------------
runtime_roc_config_p40_bdiagflip001 <- system.time(
  roc_config_p40_bdiagflip001 <- mclapply(
    list_ii_rho, 
    FUN = one_simu_ROC, 
    model = "block_diagonal",
    mc.cores = no_cores)
)
save(roc_config_p40_bdiagflip001, 
     file = paste0(path_roc, "roc_config_p40_bdiagflip001.RData"))
save(runtime_roc_config_p40_bdiagflip001, 
     file = paste0(path_roc, "runtime_roc_config_p40_bdiagflip001.RData"))

### Erdos -------------------------------------------------------------------
runtime_roc_config_p40_erdos01 <- system.time(
  roc_config_p40_erdos01 <- mclapply(
    list_ii_rho, 
    FUN = one_simu_ROC, 
    model = "erdos",
    mc.cores = no_cores)
)

save(roc_config_p40_erdos01, 
     file = paste0(path_roc, "roc_config_p40_erdos01.RData"))
save(runtime_roc_config_p40_erdos01, 
     file = paste0(path_roc, "runtime_roc_config_p40_erdos01.RData"))

### Scale-Free --------------------------------------------------------------
runtime_roc_config_p40_scalefree <- system.time(
  roc_config_p40_scalefree <- mclapply(
    list_ii_rho, 
    FUN = one_simu_ROC, 
    model = "scale_free",
    mc.cores = no_cores)
)

save(roc_config_p40_scalefree, 
     file = paste0(path_roc, "roc_config_p40_scalefree.RData"))
save(runtime_roc_config_p40_scalefree, 
     file = paste0(path_roc, "runtime_roc_config_p40_scalefree.RData"))


# Results handling --------------------------------------------------------
# See handle_roc_performances.Rmd file 

