#' Do hggm without merging X
#' @param type c("initial", "wr", "pcor", "adapt")
hggm_no_merge <- function(x,
                          lambda1 = 0,
                          fuse_thresh = 10^-3,
                          maxit = NULL,
                          distance = "euclidean",
                          lambda2_start = 1e-4,
                          lambda2_factor = 1.5,
                          precision = 1e-2,
                          weights_ = NULL,
                          type = "initial",
                          compact = TRUE) {
  p <- ncol(x)
  x <- scale(x)
  clusters <- 1:p

  t <- 1 # index for the out list.
  iter <- 0
  out <- list()
  clusters_prev <- NULL

  if (type == "wr") {
    distance <- "wr"
  } else if (type == "pcor") {
    weights_ <- weight_mat(x, "pcor")
  } else if (type == "adapt") {
    weights_ <- weight_mat(x, "adapt")
  }

  ## Loop until all the variables merged
  while (length(unique(clusters)) > 1) {
    clusters <- 1:p

    if (iter == 0) {
      beta_old <- beta_to_vector(beta_ols(x)) ## init OLS
      lambda2 <- 0
    }
    if (iter == 1) {
      lambda2 <- lambda2_start
    }

    beta <- conesta_rwrapper(x, lambda1, lambda2, beta_old,
      prec_ = precision,
      type_ = type, W_ = weights_
    )
    print(lambda1)
    beta_old <- beta_to_vector(beta)

    diffs <- dist_beta(beta, distance = distance) ## Update distance matrix

    pairs_to_merge <- which(diffs <= fuse_thresh,
      arr.ind = TRUE
    ) ## Clustering starts
    if (nrow(pairs_to_merge) != 0) {
      clusters <- merge_clusters(pairs_to_merge, clusters) # merge clusters
    } ## Clustering ends here

    lagrangian_ <- lagrangian(beta, x, lambda1, lambda2)
    cost_ <- cost(beta, x)
    cat(
      "nclusters =", length(unique(clusters)), "lambda2", lambda2,
      "lagrangian =", lagrangian_, "cost =", cost_, "\n"
    )

    if (compact) {
      if (!identical(clusters, clusters_prev)) {
        out[[t]] <- list("beta" = beta, "clusters" = clusters)
        names(out)[[t]] <- paste0("level", length(unique(clusters)))
        clusters_prev <- clusters
        t <- t + 1
      }
    } else {
      out[[t]] <- list("beta" = beta, "clusters" = clusters)
      names(out)[[t]] <- paste0("level", length(unique(clusters)))
      t <- t + 1
    }

    lambda2 <- lambda2 * lambda2_factor
    iter <- iter + 1
  }

  result <- list("out" = out, "l1" = lambda1)
  cat("niter == ", iter)

  return(result)
}

weight_mat <- function(x_, type_) {
  beta <- beta_ols(x_)

  if (type_ == "pcor") {
    w <- (1 / (1 - abs(beta)))^2 # squared because of conesta configuration
  }
  if (type_ == "adapt") {
    w <- (1 / dist_beta(beta))^2
  }

  return(w)
}


hggm_merge <- function(x,
                       lambda1 = 0,
                       fuse_thresh = 10^-3,
                       step_rule = "constant_length",
                       h = 10^-2,
                       a = 1,
                       b = 10^2,
                       maxit = NULL,
                       silent = TRUE,
                       distance = "euclidean",
                       solver = "conesta",
                       rho = 1e-2,
                       tol_admm = 1e-4,
                       lambda2_start = 1e-4,
                       lambda2_factor = 1.5,
                       precision = 1e-2,
                       weights_ = NULL,
                       type = "initial",
                       compact = TRUE) {

  ## Initialisations
  p <- ncol(x)
  x <- scale(x)
  gains <- rep(0, p - 1)
  merge <- matrix(0,
    nrow = (p - 1),
    ncol = 2
  ) # matrix of merging clusters at each level
  level <- 0
  labels <- -1:-p # vector of clusters labels
  clusters <- 1:p

  t <- 1 # index for the out list.
  iter <- 0
  out <- list()
  clusters_prev <- NULL

  if (type == "wr") {
    distance <- "wr"
  } else if (type == "pcor") {
    weights_ <- weight_mat(x, "pcor")
  } else if (type == "adapt") {
    weights_ <- weight_mat(x, "adapt")
  }
  ## End Initialisations

  ## Loop until all the variables merged
  while (length(unique(clusters)) > 1) {
    if (iter == 0) {
      beta_old <- beta_to_vector(beta_ols(x)) ## init OLS
      lambda2 <- 0
    }
    if (iter == 1) {
      lambda2 <- lambda2_start
    }

    if (solver == "conesta") {
      beta <- conesta_rwrapper(x, lambda1, lambda2, beta_old,
        prec_ = precision,
        type_ = type, W_ = weights_
      )
      print(lambda1)
      if (iter == 0) {
        old_lagrangianf <- lagrangianf <- lagrangian(beta, x, lambda1, lambda2)
      }
    }

    ## Update distance matrix
    diffs <- dist_beta(beta, distance = distance)

    ## Clustering starts here
    pairs_to_merge <- which(diffs <= fuse_thresh, arr.ind = TRUE)

    if (nrow(pairs_to_merge) != 0) {
      gain_level <- lagrangianf - old_lagrangianf
      out_mergeproc <- merge_proc(
        pairs_to_merge, clusters, x,
        beta, level, gain_level, gains, labels, merge
      )

      x <- out_mergeproc$x
      beta <- out_mergeproc$beta
      clusters <- out_mergeproc$clusters

      level <- out_mergeproc$level
      gains <- out_mergeproc$gains
      merge <- out_mergeproc$merge
      labels <- out_mergeproc$labels
    }
    beta_old <- beta_to_vector(beta)
    ## Clustering ends here

    lagrangianf <- lagrangian(beta, x, lambda1, lambda2)
    cat(
      "nclusters =", length(unique(clusters)), "lambda2",
      lambda2, "lagrangian =", lagrangianf, "\n"
    )

    gains[is.na(gains)] <- old_lagrangianf - lagrangianf
    old_lagrangianf <- lagrangianf

    if (compact) {
      if (!identical(clusters, clusters_prev)) {
        out[[t]] <- list("beta" = beta, "clusters" = clusters)
        names(out)[[t]] <- paste0("level", length(unique(clusters)))
        clusters_prev <- clusters
        t <- t + 1
      }
    } else {
      out[[t]] <- list("beta" = beta, "clusters" = clusters)
      names(out)[[t]] <- paste0("level", length(unique(clusters)))
      t <- t + 1
    }

    lambda2 <- lambda2 * lambda2_factor
    iter <- iter + 1
  }

  height <- cumsum(gains)
  tree <- list(
    merge = merge,
    height = height,
    order = rev(clusters),
    labels = paste("", 1:p),
    gains = gains
  )
  class(tree) <- "hclust"
  tree <- vegan:::reorder.hclust(tree, 1:p)

  result <- list("out" = out, "tree" = tree, "l1" = lambda1)

  return(result)
}
