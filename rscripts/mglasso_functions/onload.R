# Create relevant folders for paths if needed.

### LIBRARIES
library(capushe)
library(corpcor)
library(glasso)
library(glmnet)
library(ggplot2)
library(huge)
library(igraph)
library(latex2exp)
library(Matrix)
library(mclust)
library(mvtnorm)
library(parallel)
library(reticulate)
library(simone)
library(vegan)
library(gridExtra)
require(lattice)
require(devtools)
library(SpiecEasi)

### PATHS
path_data       <- "./data/"
path_r          <- "./rscripts/mglasso_functions/"
path_real_data  <- "./real_data/"
path_roc        <- "./roc/"

### R
setwd(path_r)
source("cost.R")
source("clustering.R")
source("mglasso.R")
source("simulate.R")
source("plot.R")
source("select_model_glasso.R")
source("perform.R")
source("normalize.R")

### PYTHON
# At first launch Install pylearn_parsimony library in the python environment
# pip install git+git://github.com/neurospin/pylearn-parsimony.git

# If needed specify the python version to be used with use_python
source_python(paste0(path_r, "solve.py"))
