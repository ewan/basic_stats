gmm <- function(mean_unif_min=-10000., mean_unif_max=10000.,
                sd_unif_max=10000., max_k=10000, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  k <- runif(1, 1, max_k)
  mean_bounds <- runif(2, mean_unif_min, mean_unif_max)
  means <- runif(k, min(mean_bounds), max(mean_bounds))
  sds <- runif(k, 0., runif(1, 0., sd_unif_max))
  probs <- runif(k, 0., 1.)
  result <- list(k=k, means=means, sds=sds, probs=probs)
  if (!is.null(seed)) set.seed(NULL)
  return(result)
}

gauss <- function(mean_unif_min=-10000., mean_unif_max=10000,
                  sd_unif_max=10000., seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  result <- list(mean=runif(1, mean_unif_min, mean_unif_max),
                 sd=runif(1, 0, sd_unif_max))
  if (!is.null(seed)) set.seed(NULL)
  return(result)
}

dist_pair_boot <- function(sample_l, ndns=512, seed=NULL) {
  g1 <- factor(sample_l$group)==levels(factor(sample_l$group))[1]
  g2 <- factor(sample_l$group)==levels(factor(sample_l$group))[2]
  sample_1 <- sample_l$x[g1]
  sample_2 <- sample_l$x[g2] 
  return(list(sample_1, sample_2))
}

dist_boot <- function(sample_l, ndns=512, seed=NULL) {
  return(sample_l$x)
}

dist_pair_matchdns <- function(sample_l, ndns=512, seed=NULL) {
  g1 <- factor(sample_l$group)==levels(factor(sample_l$group))[1]
  g2 <- factor(sample_l$group)==levels(factor(sample_l$group))[2]
  sample_1 <- sample_l$x[g1]
  sample_2 <- sample_l$x[g2] 
  overall_min <- min(c(sample_1, sample_2))
  overall_max <- max(c(sample_1, sample_2))
  d1 <- density(sample_1, from=overall_min, to=overall_max, n=ndns)
  d2 <- density(sample_2, from=overall_min, to=overall_max, n=ndns)
  return(list(d1, d2))
}

dist_matchdns <- function(sample_l, ndns=512, seed=NULL) {
  return(density(sample_l$x, from=min(sample_l$x), to=max(sample_l$x),
                 n=ndns))
}

gauss_pair_matchvar <- function(sample_l, mean_unif_min=-10000., mean_unif_max=10000,
                                seed=NULL) {
  g1 <- factor(sample_l$group)==levels(factor(sample_l$group))[1]
  g2 <- factor(sample_l$group)==levels(factor(sample_l$group))[2]
  sample_1 <- sample_l$x[g1]
  sample_2 <- sample_l$x[g2]
  sd_s <- sd(c(sample_1-mean(sample_1), sample_2-mean(sample_2)))
  mn_diff <- mean(sample_1) - mean(sample_2)
  if (!is.null(seed)) set.seed(seed)
  mn1 <- runif(1, mean_unif_min, mean_unif_max)
  mn2 <- mn1 + sample(c(-1,1), 1)*mn_diff
  if (!is.null(seed)) set.seed(NULL)
  d1 <- list(mean=mn1, sd=sd_s)
  d2 <- list(mean=mn2, sd=sd_s)
  return(list(d1, d2))  
}

gauss_matchvar <- function(sample_l, mean_unif_min=-10000., mean_unif_max=10000,
                           seed=NULL) {
  g1 <- factor(sample_l$group)==levels(factor(sample_l$group))[1]
  g2 <- factor(sample_l$group)==levels(factor(sample_l$group))[2]
  sample_1 <- sample_l$x[g1]
  sample_2 <- sample_l$x[g2]
  sd_s <- sd(c(sample_1-mean(sample_1), sample_2-mean(sample_2)))
  if (!is.null(seed)) set.seed(seed)
  result <- list(mean=runif(1, mean_unif_min, mean_unif_max), sd=sd_s)
  if (!is.null(seed)) set.seed(NULL)
  return(result)  
}

sample_gmm <- function(n, mix, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  z <- sample(1:mix$k, n, replace=T, prob=mix$probs)
  result <- sapply(z, function(i) rnorm(1, mix$means[i], mix$sds[i]))
  if (!is.null(seed)) set.seed(NULL)
  return(result)
}

sample_gauss <- function(n, gd, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  result <- rnorm(n, gd$mean, gd$sd)
  if (!is.null(seed)) set.seed(NULL)
  return(result)
}

sample_dns <- function(n, dns, seed=NULL) {
  k <- length(dns$x)
  pts <- dns$x
  d <- dns$y
  bw <- dns$bw
  m <- list(k=k, means=pts, sds=rep(bw, k), probs=d)
  return(sample_gmm(n, m, seed))
}

sample_boot <- function(n, s, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  result <- sample(s, size = n, replace=T)
  if (!is.null(seed)) set.seed(NULL)
  return(result)
}

sample_2groups_same <- function(n_g1, n_g2, mix, sample_fn, seed=NULL) {
  result <- data.frame(group=c(rep("Group 1", n_g1),
                               rep("Group 2", n_g2)),
                       x=sample_fn(n_g1+n_g2, mix, seed),
                       stringsAsFactors=F)
  return(result)
}

sample_2groups_diff <- function(n_g1, n_g2, mix1, mix2, sample_fn, seed=NULL) {
  result <- data.frame(group=c(rep("Group 1", n_g1),
                               rep("Group 2", n_g2)),
                       x=c(sample_fn(n_g1, mix1, seed),
                           sample_fn(n_g2, mix2, seed)),
                       stringsAsFactors=F)
  return(result)
}


sample_2groups <- function(same_dists, diff_dists, n_g1, n_g2,
                           dist_sample, seed=NULL, parallel=T) {
  n_exp <- length(same_dists)
  same <- ldply(1:n_exp, function(i) {
    if (!is.null(seed)) {
    r <- sample_2groups_same(n_g1, n_g2, same_dists[[i]], dist_sample, seed=seed+i)
    } else {
    r <- sample_2groups_same(n_g1, n_g2, same_dists[[i]], dist_sample, seed=NULL)
    }
    r$iter <- paste0("Iter", i)
    return(r)
  },
  .parallel=parallel)
  same$theory <- "Same"
  diff <- ldply(1:n_exp, function(i) {
    if (!is.null(seed)) {
    r <- sample_2groups_diff(n_g1, n_g2,
                             diff_dists[[i]][[1]],
                             diff_dists[[i]][[2]],
                             dist_sample,
                             seed=seed+i)
    } else {
    r <- sample_2groups_diff(n_g1, n_g2,
                             diff_dists[[i]][[1]],
                             diff_dists[[i]][[2]],
                             dist_sample,
                             seed=NULL)
    }
    r$iter <- paste0("Iter", i)
    return(r)
  },
  .parallel=parallel)  
  diff$theory <- "Different"
  result <- rbind(same, diff)   
  return(result)
}

sample_2groups_samediff <- function(n_exp, n_g1, n_g2, dist_gen, dist_sample, seed=NULL, parallel=T, ...) {
  if (parallel) registerDoParallel()
  if (!is.null(seed)) {
    same_dist <- foreach(i=1:n_exp) %dopar% dist_gen(seed=seed+i, ...)
    diff_dist <- foreach(i=1:n_exp) %dopar% list(dist_gen(seed=seed+2*i, ...),
                                               dist_gen(seed=seed+3*i, ...))
  } else {
    same_dist <- foreach(i=1:n_exp) %dopar% dist_gen(seed=NULL, ...)
    diff_dist <- foreach(i=1:n_exp) %dopar% list(dist_gen(seed=NULL, ...),
                                               dist_gen(seed=NULL, ...))
  }
  return(sample_2groups(same_dist, diff_dist, n_g1, n_g2, dist_sample, seed=seed,
                        parallel=parallel))
}

sample_2groups_fromsample <- function(n_exp, sample_l, dist_gen_same, dist_gen_diff,
                                      dist_sample, seed=NULL, parallel=T, ...) {
  if (parallel) registerDoParallel()
  if (!is.null(seed)) {
  same_dist <- foreach(i=1:n_exp) %dopar% dist_gen_same(sample_l=sample_l, seed=seed+i, ...)
  diff_dist <- foreach(i=1:n_exp) %dopar% dist_gen_diff(sample_l=sample_l, seed=seed+i, ...)
  } else {
  same_dist <- foreach(i=1:n_exp) %dopar% dist_gen_same(sample_l=sample_l, seed=NULL, ...)
  diff_dist <- foreach(i=1:n_exp) %dopar% dist_gen_diff(sample_l=sample_l, seed=NULL, ...)
  }
  n_g1 <- sum(factor(sample_l$group)==levels(factor(sample_l$group))[1])
  n_g2 <- sum(factor(sample_l$group)==levels(factor(sample_l$group))[2])
  return(sample_2groups(same_dist, diff_dist, n_g1, n_g2, dist_sample, seed=seed,
                        parallel=parallel))
}