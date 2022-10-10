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
remotes::install_github("jchiquet/simone")
remotes::install_github("zdk123/SpiecEasi")
remotes::install_github("ewenme/ghibli")
BiocManager::install("phyloseq")
## ____________________________________________________
## Additional R packages needed by the user (CRAN)
## example: remote
## install.packages("anRpackage")
## ____________________________________________________

## ____________________________________________________
## Additional R packages needed by the user ()
## remotes::install_github("user/package")
## ____________________________________________________
