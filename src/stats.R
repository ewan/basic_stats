tstat <- function(x1, x2) {
  n1 <- length(x1)
  n2 <- length(x2)
  denom <- sqrt((n1-1)*var(x1)/(n1^2) + (n2-1)*var(x2)/(n2^2))
  return(meandiff(x1, x2)/denom)
}

abststat <- function(x1, x2) abs(tstat(x1, x2))

tstat_by <- function(x, group) {
  group <- factor(group)
  return(tstat(x[group==levels(group)[1]],x[group==levels(group)[2]]))
}

abststat_by <- function(x, group) {
  group <- factor(group)
  return(abststat(x[group==levels(group)[1]],x[group==levels(group)[2]]))
}

meandiff <- function(x1, x2) {
  return(mean(x1)-mean(x2))
}

meandiff_by <- function(x, group) {
  group <- factor(group)
  return(meandiff(x[group==levels(group)[1]],x[group==levels(group)[2]]))
}

absmeandiff <- function(x1, x2) {
  return(abs(meandiff(x1, x2)))
}

absmeandiff_by <- function(x, group) {
  group <- factor(group)
  return(absmeandiff(x[group==levels(group)[1]],x[group==levels(group)[2]]))
}

absmeandiff_sd <- function(x1, x2) {
  return(absmeandiff(x1, x2)/sd(c(x1, x2)))
}

absmeandiff_sd_by <- function(x, group) {
  group <- factor(group)
  return(absmeandiff_sd(x[group==levels(group)[1]],x[group==levels(group)[2]]))
}

meandiff_sd <- function(x1, x2) {
  return(meandiff(x1, x2)/sd(c(x1, x2)))
}

meandiff_sd_by <- function(x, group) {
  group <- factor(group)
  return(meandiff_sd(x[group==levels(group)[1]],x[group==levels(group)[2]]))
}

skld <- function(x1, x2) {
  min_all <- min(c(x1,x2))
  max_all <- max(c(x1,x2))
  dns_1 <- stats::density(x1, from=min_all, to=max_all)
  dns_2 <- stats::density(x2, from=min_all, to=max_all)
  ndns_1 <- dns_1$y/sum(dns_1$y)
  ndns_2 <- dns_2$y/sum(dns_2$y)
  lndns_1 <- log(ndns_1)
  lndns_2 <- log(ndns_2)
  lndns_1[lndns_1==-Inf] <- 0
  lndns_2[lndns_2==-Inf] <- 0
  kld_1 <- sum(ndns_1*(lndns_1-lndns_2))
  kld_2 <- sum(ndns_2*(lndns_2-lndns_1))
  return(mean(kld_1, kld_2))
}

skld_by <- function(x, group) {
  group <- factor(group)
  return(skld(x[group==levels(group)[1]],x[group==levels(group)[2]]))
}

qrank <- function(x) {
  r <- rank(x)
  rank_p <- (2*r-1)/(2*max(r))
  return(qnorm(rank_p))
}