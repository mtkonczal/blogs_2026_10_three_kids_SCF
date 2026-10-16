# 27_period_change_test.R
# Design-based test of the change between 1989-1998 and 2013-2025 in the share of families with
# 3+ children: top 1%, 90th-99th, and their gap. Heads 45-64. Same estimator as R/25:
#  - Ranks within survey year, implicate and 5-year age band (as in R/07); each wave weighted
#    equally within its period; pooled share = ratio of summed moments.
#  - Ranks re-estimated inside every replicate (999 Fed replicate weights, implicate 1).
#  - Waves are independent samples, so per-wave replicate variances add, within and across periods.
#  - Imputation variance 1.2 B on the period difference itself, across the five implicates.
# Three rankings: the production income measure (usual income, actual income in 1989 and 1992,
# when the usual-income amount was not collected), actual income in every wave, and net worth.
# Replicate weights: data_raw/rw/p{YY}_rw1.dta for 1989-1998 and 2013-2025.
# The 2013-2025 production-income gap must reproduce R/25 (all_gap3).
source("R/scf_functions.R")
suppressPackageStartupMessages({ library(haven) })

periods <- list(early = c(1989, 1992, 1995, 1998), recent = seq(2013, 2025, 3))
main <- readRDS("output/kids_main.rds")[year %in% unlist(periods)]
setorder(main, year, Y1)   # same tie order as R/07 and R/25
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); r[o] <- (cumsum(w[o]) - w[o] / 2) / sum(w); r }
rank_age <- function(x, w, age5) { r <- numeric(length(x)); for (a in unique(age5)) { j <- which(age5 == a); r[j] <- prank(x[j], w[j]) }; r }
# Moments (den_top, num_top, den_aff, num_aff), weights normalized within the wave.
moments <- function(y3, w, r) {
  w <- w / sum(w); top <- r > .99; aff <- r > .9 & r <= .99
  c(sum(w[top]), sum(w[top] * y3[top]), sum(w[aff]), sum(w[aff] * y3[aff]))
}
statfun <- function(m) { a <- 100 * c(m[2] / m[1], m[4] / m[3]); c(top = a[1], aff = a[2], gap = a[1] - a[2]) }
jac <- function(m) sapply(1:4, function(j) { h <- 1e-7; u <- d <- m; u[j] <- u[j] + h; d[j] <- d[j] - h; (statfun(u) - statfun(d)) / (2 * h) })

# Replicate weights for implicate 1, aligned with each wave's sample rows.
rw_mat <- function(yr, ids) {
  rw <- as.data.table(zap_labels(read_dta(sprintf("data_raw/rw/p%02d_rw1.dta", yr %% 100))))
  setnames(rw, tolower(names(rw)))
  idc <- if ("y1" %in% names(rw)) "y1" else "x1"   # 1989 file uses X1 (renamed Y1 in R/02)
  ix <- match(ids, rw[[idc]])
  if (anyNA(ix)) stop("replicate weights missing for some implicate-1 cases in ", yr)
  m <- sapply(1:999, function(b) rw[[paste0("wt1b", b)]][ix] * rw[[paste0("mm", b)]][ix])
  m[is.na(m)] <- 0
  m
}
waves <- unlist(periods)
# note: main has a column named y, so loop variables here are called wv
RW <- setNames(lapply(waves, function(wv) rw_mat(wv, main[year == wv & implicate == 1, Y1])), waves)
main[, y3 := as.numeric(kids >= 3)]
main[, actinc := INCOME]

run <- function(v) {
  per <- lapply(periods, function(ys) {
    # point estimates per implicate
    M <- lapply(1:5, function(i) Reduce(`+`, lapply(ys, function(wv) {
      s <- main[year == wv & implicate == i]; moments(s$y3, s$WGT, rank_age(s[[v]], s$WGT, s$age5)) })))
    # replicate draws per wave (implicate 1)
    D <- lapply(ys, function(wv) {
      s <- main[year == wv & implicate == 1]; R <- RW[[as.character(wv)]]
      sapply(1:999, function(b) moments(s$y3, R[, b], rank_age(s[[v]], R[, b], s$age5)))
    })
    list(Q = sapply(M, statfun), J = jac(M[[1]]), D = D)
  })
  # sampling variance of each statistic within a period: sum over independent waves
  Vp <- lapply(per, function(p) Reduce(`+`, lapply(p$D, function(d) apply(p$J %*% d, 1, var))))
  out <- list()
  for (nm in names(periods)) {
    Q <- per[[nm]]$Q; se <- sqrt(Vp[[nm]] + 1.2 * apply(Q, 1, var))
    out[[nm]] <- data.table(period = nm, stat = rownames(Q), est = rowMeans(Q), se = se)
  }
  Qd <- per$recent$Q - per$early$Q
  se <- sqrt(Vp$early + Vp$recent + 1.2 * apply(Qd, 1, var))
  out$change <- data.table(period = "change", stat = rownames(Qd), est = rowMeans(Qd), se = se)
  res <- rbindlist(out)
  res[, `:=`(rank = v, lower = est - 1.96 * se, upper = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))]
  res[]
}
res <- rbindlist(lapply(c("inc_measure", "actinc", "NETWORTH"), function(v) { message("ranking by ", v); run(v) }))
res[, rank := c(inc_measure = "income (production)", actinc = "actual income", NETWORTH = "net worth")[rank]]

# Sanity check: recent-period gaps must match R/25.
r25 <- readRDS("output/kids_rep5.rds")[stat == "all_gap3"]
chk <- rbind(data.table(rank = "income (production)", r25 = r25[rank == "inc_rank"]),
             data.table(rank = "net worth", r25 = r25[rank == "nw_rank"]))
chk <- merge(res[period == "recent" & stat == "gap", .(rank, est, se)], chk[, .(rank, r25_est = r25.est, r25_se = r25.se)], by = "rank")
print(chk)
stopifnot(chk[, all(abs(est - r25_est) < 1e-6 & abs(se - r25_se) < 0.05)])

attr(res, "built_at") <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
saveRDS(res, "output/kids_period_change.rds")
options(width = 200)
print(res[, .(rank, period, stat, est = round(est, 2), se = round(se, 2), lower = round(lower, 2), upper = round(upper, 2), p = signif(p, 3))])
message("Wrote output/kids_period_change.rds")
