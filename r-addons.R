## ====================================================
## R packages needed not available via mini-conda
## ====================================================

## ____________________________________________________
## Setting-up the source repository
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
})
## ____________________________________________________
reticulate::py_install(packages = c("pylearn-parsimony"), 
                       envname = "computo", 
                       method = "conda", 
                       conda = "auto", 
                       python_version = 3.8, 
                       pip = TRUE, 
                       pip_options = 'git+https://github.com/neurospin/pylearn-parsimony.git')

# if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::hasFun("restartSession"))
#   rstudioapi::restartSession()
# 
# reticulate::py_config()

remotes::install_github("jchiquet/simone")
remotes::install_github("zdk123/SpiecEasi")
remotes::install_github("ewenme/ghibli")
BiocManager::install("phyloseq")
install.packages("parallel")

## ____________________________________________________
## Additional R packages needed by the user (CRAN)
## example: remote
## install.packages("anRpackage")

## ____________________________________________________

## ____________________________________________________
## Additional R packages needed by the user ()
## remotes::install_github("user/package")
## ____________________________________________________
