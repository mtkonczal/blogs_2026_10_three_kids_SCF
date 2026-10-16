# 28_bin_intervals.R
# Replicate-weight 95% intervals for every percentile bin on the income and wealth charts
# (share with 3+ children and average children), heads 45-64, pooled 2013-2025.
# Same estimator as R/25, applied bin by bin:
#  - Ranks within survey year, implicate and 5-year age band, re-estimated inside every replicate
#    (999 Fed replicate weights, implicate 1), so bin membership uncertainty is included.
#  - Equal weight per wave; pooled bin value = ratio of summed moments.
#  - Waves independent: per-wave replicate variances summed through the ratio's gradient.
#  - Imputation variance 1.2 B across the five implicates.
# These are pointwise intervals for each bin, not tests of differences between bins.
# Point estimates must equal the charted values; top-bin SEs must match R/25 (all_top3, all_topkids).
# Writes output/kids_bin_ci.rds, read by R/22.
source("R/scf_functions.R")
suppressPackageStartupMessages({ library(haven) })

years <- seq(2013, 2025, 3)
main <- readRDS("output/kids_main.rds")[year %in% years]
setorder(main, year, Y1)   # same tie order as R/07 and R/25
main[, y3 := as.numeric(kids >= 3)]
bin_lv <- levels(main$inc_bin)
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); r[o] <- (cumsum(w[o]) - w[o] / 2) / sum(w); r }
rank_age <- function(x, w, age5) { r <- numeric(length(x)); for (a in unique(age5)) { j <- which(age5 == a); r[j] <- prank(x[j], w[j]) }; r }
# Same bins as R/07: right-closed intervals at the decile cutoffs, then 95 and 99.
# pmin: with zero replicate weights at the top, cumsum rounding can put a rank a hair above 1;
# R/25 counts those families in the top 1% (rank > .99), so cap at 1 to match.
binit <- function(r) factor(as.character(cut(pmin(r, 1), c(0, seq(.1, .9, .1), .95, .99, 1), labels = bin_lv, include.lowest = TRUE)), bin_lv)
# Moments by bin: weight, weighted 3+ count, weighted child count; weights normalized within the wave.
moments <- function(s, w, r) {
  w <- w / sum(w); b <- binit(r)
  m <- rowsum(cbind(w, w * s$y3, w * s$kids), b, reorder = TRUE)
  out <- matrix(0, length(bin_lv), 3, dimnames = list(bin_lv, c("den", "n3", "nk")))
  out[rownames(m), ] <- m
  out
}
stat <- function(m) cbind(share3 = 100 * m[, "n3"] / m[, "den"], kids = m[, "nk"] / m[, "den"])

RW <- lapply(years, function(wv) {
  rw <- as.data.table(zap_labels(read_dta(sprintf("data_raw/rw/p%02d_rw1.dta", wv %% 100))))
  setnames(rw, tolower(names(rw)))
  ix <- match(main[year == wv & implicate == 1, Y1], rw$y1)
  if (anyNA(ix)) stop("replicate weights missing for some implicate-1 cases in ", wv)
  m <- sapply(1:999, function(b) rw[[paste0("wt1b", b)]][ix] * rw[[paste0("mm", b)]][ix])
  m[is.na(m)] <- 0
  m
})

run <- function(v) {
  # point estimates, implicate by implicate (ranks recomputed; identical to R/07's)
  M <- lapply(1:5, function(i) Reduce(`+`, lapply(years, function(wv) {
    s <- main[year == wv & implicate == i]; moments(s, s$WGT, rank_age(s[[v]], s$WGT, s$age5)) })))
  Q <- simplify2array(lapply(M, stat))   # bins x 2 stats x 5 implicates
  base <- M[[1]]
  # gradient of N/D with respect to (den, num): (-N/D^2, 1/D), per bin
  gd3 <- -100 * base[, "n3"] / base[, "den"]^2; gn3 <- 100 / base[, "den"]
  gdk <- -base[, "nk"] / base[, "den"]^2;       gnk <- 1 / base[, "den"]
  V <- matrix(0, length(bin_lv), 2)
  for (k in seq_along(years)) {
    s <- main[year == years[k] & implicate == 1]; R <- RW[[k]]
    lin <- array(0, c(length(bin_lv), 2, 999))
    for (b in 1:999) {
      m <- moments(s, R[, b], rank_age(s[[v]], R[, b], s$age5))
      lin[, 1, b] <- gd3 * m[, "den"] + gn3 * m[, "n3"]
      lin[, 2, b] <- gdk * m[, "den"] + gnk * m[, "nk"]
    }
    V <- V + apply(lin, c(1, 2), var)   # var() divides by 998, the Fed's convention
  }
  B <- apply(Q, c(1, 2), var)
  est <- apply(Q, c(1, 2), mean)
  se <- sqrt(V + 1.2 * B)
  rbind(data.table(rank = v, outcome = "share3", inc_bin = factor(bin_lv, bin_lv), est = est[, 1], se = se[, 1]),
        data.table(rank = v, outcome = "kids", inc_bin = factor(bin_lv, bin_lv), est = est[, 2], se = se[, 2]))
}
out <- rbind(run("inc_measure"), run("NETWORTH"))
out[, rank := fifelse(rank == "inc_measure", "inc_rank", "nw_rank")]

# Sanity checks: top-bin values and SEs match R/25.
r25 <- readRDS("output/kids_rep5.rds")
chk <- merge(out[inc_bin == "Top 1%", .(rank, stat = fifelse(outcome == "share3", "all_top3", "all_topkids"), est, se)],
             r25[, .(rank, stat, est25 = est, se25 = se)], by = c("rank", "stat"))
print(chk)
stopifnot(nrow(chk) == 4, chk[, all(abs(est - est25) < 1e-6 & abs(se - se25) < 1e-3 * pmax(1, se25))])

attr(out, "built_at") <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
saveRDS(out, "output/kids_bin_ci.rds")
options(width = 200)
print(dcast(out, rank + inc_bin ~ outcome, value.var = c("est", "se")), digits = 3)
message("Wrote output/kids_bin_ci.rds")
