# SUPPORT RECOVERY --------------------------------------------------------

library(simone)
library(ghibli)
library(phyloseq)
library(rmarkdown)
library(knitr)

library(parallel)
library(huge)
library(mglasso)
mglasso::install_pylearn_parsimony(envname = "rmglasso", method = "conda") ## if pylearn_parsimony not installed
reticulate::use_condaenv("rmglasso", required = TRUE)
reticulate::py_config()
path_data <- "./data/"

## Erdos Performances calculation ------------------------------------------------
## Launched on a cluster using 72 cores

### Settings ----------------------------------------------------------------
### Model -------------------------------------------------------------------
## NA values for some parameter mean they are not relevant
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
no_cores    <- min(detectCores() - 2, length(list_ii_rho)) ## update according to the 
## number of available cores 

### Erdos -------------------------------------------------------------------
path_roc <- path_data # path for saving results 
runtime_roc_config_p40_erdos01 <- system.time(
  roc_config_p40_erdos01 <- mclapply(
    list_ii_rho, 
    FUN = one_simu_ROC, 
    model = "erdos",
    mc.cores = no_cores)
)

save(roc_config_p40_erdos01, 
     file = paste0(path_data, "roc_config_p40_erdos01.RData"))
save(runtime_roc_config_p40_erdos01, 
     file = paste0(path_data, "runtime_roc_config_p40_erdos01.RData"))

load(paste0(path_data, "roc_config_p40_erdos01.RData")) 
dt_full <- roc_config_p40_erdos01

#### Merge in one graph
# Three sample sizes are used and the vector c(20,40,80) is replicated 50 times
# subset the dataframe in three parts corresponding to the relevant sample sizes
index <- seq(1, 150, by = 3)
roc_dt20 <- dt_full[index]

index <- seq(2, 150, by = 3)
roc_dt40 <- dt_full[index]

index <- seq(3, 150, by = 3)
roc_dt80 <- dt_full[index]

# compute the mean over the 50 ROC curves
roc_dt20 <- get_mean_ROC_stat(roc_dt20)
roc_dt40 <- get_mean_ROC_stat(roc_dt40)
roc_dt80 <- get_mean_ROC_stat(roc_dt80)

# restructure the list result in a matrix for plot
roc_dt20 <- reformat_roc_res_for_ggplot(roc_dt20)
roc_dt20$np <- 0.5 # add a ratio n over p variable
roc_dt40 <- reformat_roc_res_for_ggplot(roc_dt40)
roc_dt40$np <- 1
roc_dt80 <- reformat_roc_res_for_ggplot(roc_dt80)
roc_dt80$np <- 2

roc_dtf_erdos <- rbind(roc_dt20, roc_dt40, roc_dt80)

save(roc_dtf_erdos, file = paste0(path_data, "roc_dtf_erdos.RData"))

## Scale free Performances calculation -----------------------------------------
# Launched on a cluster using 72 cores

### Settings ----------------------------------------------------------------
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


### Scale-Free --------------------------------------------------------------
runtime_roc_config_p40_scalefree <- system.time(
  roc_config_p40_scalefree <- mclapply(
    list_ii_rho, 
    FUN = one_simu_ROC, 
    model = "scale_free",
    mc.cores = no_cores)
)

save(roc_config_p40_scalefree, 
     file = paste0(path_data, "roc_config_p40_scalefree.RData"))
save(runtime_roc_config_p40_scalefree, 
     file = paste0(path_data, "runtime_roc_config_p40_scalefree.RData"))

load(paste0(path_data, "roc_config_p40_scalefree.RData")) 
dt_full <- roc_config_p40_scalefree

### Merge in one graph
index <- seq(1, 150, by = 3)
roc_dt20 <- dt_full[index]

index <- seq(2, 150, by = 3)
roc_dt40 <- dt_full[index]

index <- seq(3, 150, by = 3)
roc_dt80 <- dt_full[index]

roc_dt20 <- get_mean_ROC_stat(roc_dt20)
roc_dt40 <- get_mean_ROC_stat(roc_dt40)
roc_dt80 <- get_mean_ROC_stat(roc_dt80)

roc_dt20 <- reformat_roc_res_for_ggplot(roc_dt20)
roc_dt20$np <- 0.5
roc_dt40 <- reformat_roc_res_for_ggplot(roc_dt40)
roc_dt40$np <- 1
roc_dt80 <- reformat_roc_res_for_ggplot(roc_dt80)
roc_dt80$np <- 2

roc_dtf_sfree <- rbind(roc_dt20, roc_dt40, roc_dt80)

### Save
save(roc_dtf_sfree,
     file = paste0(path_data, "roc_dtf_sfree.RData"))


## SBM Performances calculation -----------------------------------------
# Launched on a cluster using 72 cores
### Settings ----------------------------------------------------------------
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

### Stochastic Block Diagonal -----------------------------------------------
runtime_roc_config_p40_bdiagflip001 <- system.time(
  roc_config_p40_bdiagflip001 <- mclapply(
    list_ii_rho, 
    FUN = one_simu_ROC, 
    model = "block_diagonal",
    mc.cores = no_cores)
)
save(roc_config_p40_bdiagflip001, 
     file = paste0(path_data, "roc_config_p40_bdiagflip001.RData"))
save(runtime_roc_config_p40_bdiagflip001, 
     file = paste0(path_data, "runtime_roc_config_p40_bdiagflip001.RData"))

load(paste0(path_data, "roc_config_p40_bdiagflip001.RData")) 
dt_full <- roc_config_p40_bdiagflip001

### Merge in one graph
index <- seq(1, 150, by = 3)
roc_dt20 <- dt_full[index]

index <- seq(2, 150, by = 3)
roc_dt40 <- dt_full[index]

index <- seq(3, 150, by = 3)
roc_dt80 <- dt_full[index]

roc_dt20 <- get_mean_ROC_stat(roc_dt20)
roc_dt40 <- get_mean_ROC_stat(roc_dt40)
roc_dt80 <- get_mean_ROC_stat(roc_dt80)

roc_dt20 <- reformat_roc_res_for_ggplot(roc_dt20)
roc_dt20$np <- 0.5
roc_dt40 <- reformat_roc_res_for_ggplot(roc_dt40)
roc_dt40$np <- 1
roc_dt80 <- reformat_roc_res_for_ggplot(roc_dt80)
roc_dt80$np <- 2

roc_dtf_sbm <- rbind(roc_dt20, roc_dt40, roc_dt80)

save(roc_dtf_sbm,
     file = paste0(path_data, "roc_dtf_sbm.RData"))


# CLUSTERING --------------------------------------------------------------
library(capushe)
library(mclust)
## Correlation set to 0.25 -------------------------------------------------
### Settings ----------------------------------------------------------------
p         <- 40
seq_n     <- c(20, 40, 80) 
alpha     <- rep(1/5, 5)
seq_rho   <- c(0.25, 0.95)
seq_dnsty <- c(0.75)
type      <- NA     #1 ## unused to do: delete in configs_simu parameters
ngroup    <- length(alpha)
pi        <- diag(0.75, ngroup)

n_simu      <- 100
list_ii_rho <- configs_simu(n_simu, seq_rho, seq_dnsty, seq_n, type)
mc_cores    <- min(detectCores() - 2, length(list_ii_rho)) ## update according to the 
path_extended <- path_data
## number of available cores 
RNGkind("L'Ecuyer-CMRG")

### Stochastic Block Diagonal -----------------------------------------------
runtime_rand100_config_p40_bdiagflip001_cor025 <- system.time(
  rand100_config_p40_bdiagflip001_cor025 <- mclapply(
    list_ii_rho, 
    FUN = one_simu_extended, 
    model = "block_diagonal",
    mc.cores = mc_cores)
)

save(rand100_config_p40_bdiagflip001_cor025, 
     file = paste0(path_data, "rand100_config_p40_bdiagflip001_cor025.RData"))

## Correlation parameter set to 0.95 ---------------------------------------

### Settings ----------------------------------------------------------------
p         <- 40
seq_n     <- c(20, 40, 80)
alpha     <- rep(1 / 5, 5)
seq_rho   <- c(0.95)
seq_dnsty <- c(0.75)
type      <-
  NA     #1 ## unused to do: delete in configs_simu parameters
ngroup    <- length(alpha)
pi        <- diag(0.75, ngroup)

n_simu      <- 100
list_ii_rho <- configs_simu(n_simu, seq_rho, seq_dnsty, seq_n, type)
mc_cores    <- min(80, length(list_ii_rho))
RNGkind("L'Ecuyer-CMRG")

### Stochastic Block Diagonal -----------------------------------------------
runtime_rand100_config_p40_bdiagflip001_cor95 <- system.time(
  rand100_config_p40_bdiagflip001_cor95 <- mclapply(
    list_ii_rho, 
    FUN = one_simu_extended, 
    model = "block_diagonal",
    mc.cores = mc_cores)
)

save(rand100_config_p40_bdiagflip001_cor95, 
     file = paste0(path_data, "rand100_config_p40_bdiagflip001_cor95.RData"))

# Clustering  
# Settings:  
# - Inter-clusters edge probability $0.01$ (flip on all the missing edges)  

## Rand index  
load(paste0(path_data, "rand100_config_p40_bdiagflip001_cor025.RData")) 
dt <- rand100_config_p40_bdiagflip001_cor025

# Calculate clusters partitions with thresh_fuse as the required difference
# threshold for merging two regression vectors
list_res <-
  lapply(dt, function(e) {
    get_perf_from_raw("rand", e, thresh_fuse = 1e-6)
  })
dt_rand <- do.call(rbind, list_res)

save(dt_rand,
     file = paste0(path_data, "rand_dt_lower_cor_sbm.RData"))

# Clustering  
# Settings:  
# - Inter-clusters edge probability $0.01$ (flip on all the missing edges)  

## Rand index  
load(paste0(path_data, "rand100_config_p40_bdiagflip001_cor95.RData")) 
dt <- rand100_config_p40_bdiagflip001_cor95

# Calculate clusters partitions with thresh_fuse as the required difference
# threshold for merging two regression vectors
list_res <-
  lapply(dt, function(e) {
    get_perf_from_raw("rand", e, thresh_fuse = 1e-6)
  })
dt_rand <- do.call(rbind, list_res)

save(dt_rand,
     file = paste0(path_data, "rand_dt_higher_cor_sbm.RData"))


# APPLICATIONS ------------------------------------------------------------


## microbial associations in gut data -------------------------------------


library(SpiecEasi)

#Load  
load(paste0(path_data, "amgut1.filt.rda"))
dta <- amgut1.filt

#Transform data using `clr` approach or non paranormal method
dta <- t(clr(dta + 1 , 1))

#Stars selection  
mb_out      <- neighbor_select(data = dta, lambda_min = 1e-2, nlambda = 20, 
                               nresamples = 50, verbose = TRUE, estim_var = 0.05)
lambda1     <- mb_out$lambda_opt

## Sequence 1 of lambda2 values 
pen_params <- seq_l2_l1_fixed(dt = dta, l1 = lambda1, nl2 = 20, l2_max = TRUE) 
system.time(mgl_amgut_rev <- lapply(pen_params, 
                                    FUN = mglasso_pair_param, 
                                    X_ = dta, 
                                    type = "initial"))
save(mgl_amgut_rev, 
     file = paste0(path_real_data, "mgl_amgut_rev_l2_seq0to1_20val.RData"))

## Sequence 2 of lambda2 values 
pen_params <- seq_l2_l1_fixed(dt = dta, l1 = lambda1, nl2 = 20, l2_max = 20) 
pen_params[[1]] <- NULL # remove l2 = 0, already computed in prev list
system.time(mgl_amgut_rev_set2 <- mclapply(pen_params, 
                                           FUN = mglasso_pair_param, 
                                           X_ = dta, 
                                           type = "initial",
                                           mc.cores = 19))
save(mgl_amgut_rev_set2, 
     file = paste0(path_real_data, "mgl_amgut_rev_l2_seq1to20_20val.RData"))

## Sequence 3 of lambda2 values 
pen_params <- seq_l2_l1_fixed(dt = dta, l1 = lambda1, nl2 = 20, l2_max = 4.3) 
pen_params[[1]] <- NULL # remove l2 = 0, already computed in prev list
system.time(mgl_amgut_rev_set3 <- mclapply(pen_params, 
                                           FUN = mglasso_pair_param, 
                                           X_ = dta, 
                                           type = "initial",
                                           mc.cores = 19))
save(mgl_amgut_rev_set3, 
     file = paste0(path_real_data, "mgl_amgut_rev_l2_seq0to4_20val.RData"))

## methylation and transcriptomic genotypes in poplar ----------------------

# Data
epit_sparse <- readRDS(paste0(path_data, "epit-spca-select.rds"))

#Selection of sparsity penalty parameter
mb_out      <-
  neighbor_select(
    data = epit_sparse,
    lambda_min = 1e-3,
    nlambda = 50,
    nresamples = 50,
    verbose = TRUE,
    estim_var = 0.05
  )

lambda1_genot     <- mb_out$lambda_opt

pen_params_genot <-
  seq_l2_l1_fixed(
    dt = epit_sparse,
    l1 = lambda1_genot,
    nl2 = 20,
    l2_max = 30.94
  )

mgl_epit_sparse_geno <- lapply(pen_params_genot,
                               FUN = mglasso_pair_param,
                               X_ = epit_sparse,
                               type = "initial")

saveRDS(paste0(path_data, "mgl_epit_sparse_geno.rds"))