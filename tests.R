path_functions <- "./functions/"

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::hasFun("restartSession"))
  rstudioapi::restartSession()

path_to_python <- reticulate::conda_python("computo")
writeLines(sprintf("RETICULATE_PYTHON=%s", path_to_python),
           Sys.getenv("GITHUB_ENV"))
reticulate::py_config()

# To check if all required python dependencies are available.
testthat::expect_true(reticulate::py_module_available("numpy"))
testthat::expect_true(reticulate::py_module_available("scipy"))
testthat::expect_true(reticulate::py_module_available("six"))
testthat::expect_true(reticulate::py_module_available("matplotlib"))
testthat::expect_true("scikit-learn" %in% reticulate::py_list_packages()$package)
testthat::expect_true("pylearn-parsimony" %in% reticulate::py_list_packages()$package)

reticulate::source_python(paste0(path_functions, "solve.py"))

# Example test mglasso solver function 
n = 30
K = 2
p = 4
rho = 0.85
blocs <- list()
for (j in 1:K) {
  bloc <- matrix(rho, nrow = p/K, ncol = p/K)
  for(i in 1:(p/K)) { bloc[i,i] <- 1 }
  blocs[[j]] <- bloc
}
mat.covariance <- Matrix::bdiag(blocs)
mat.covariance
set.seed(11)
X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
X <- scale(X)

res_conesta <- conesta_rwrapper(X, 0.1, 0.1)
res_conesta

source(paste0(path_functions, "mglasso.R"))
# example path algorithm result
res_mglasso <-
  hggm_no_merge(
    X,
    lambda1 = 0,
    fuse_thresh = 1e-3,
    maxit = 1e3,
    distance = "euclidean",
    lambda2_start = 1e-2,
    lambda2_factor = 1.5,
    precision = 1e-2,
    weights_ = NULL,
    type = "initial",
    compact = TRUE)
res_mglasso