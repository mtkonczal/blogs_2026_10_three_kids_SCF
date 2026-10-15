# 25_rep_inference.R
# Design-based significance tests for the headline gaps, on the same five pooled waves
# (2013-2025) that the charts show. Heads 45-64. Ported from audit/audit_checks.R, which an
# independent audit used to replace the earlier three-wave (2019-2025) tests.
#
# Statistics (top 1% vs. 90th-99th, and vs. 40th-60th), for income and net-worth ranks:
#   share with 3+ children, average children, share with exactly 3 and with 4+,
#   for all families, excluding remarried couples, and first-marriage couples only.
#
# Why this differs from rep_stat() in R/rep_se.R:
#  - Pooled estimator. Each wave's weights are normalized to sum to 1 (equal weight per wave,
#    as in the charts), so the pooled share is a ratio of summed moments, not an average of
#    per-wave gaps. Point estimates therefore equal the charted 2013-2025 levels.
#  - Ranks are re-estimated inside every replicate (within age band, implicate 1), because
#    "top 1%" is itself an estimated percentile. Holding ranks fixed ignores that
#    uncertainty (for the main comparisons here it gives smaller SEs).
#  - Waves are independent samples: per-wave replicate variances of the moments are combined
#    through the Jacobian of the pooled ratios (delta method). Imputation variance (1.2 B) is
#    computed on the pooled statistic across the five implicates.
# Replicate weights: data_raw/rw/p{YY}_rw1.dta for 2013, 2016, 2019, 2022, 2025.
source("R/scf_functions.R")
suppressPackageStartupMessages({ library(haven) })
main <- readRDS("output/kids_main.rds")
rec <- main[year >= 2013]
setorder(rec, year, Y1)   # deterministic tie order for the within-replicate re-ranking
stopifnot(identical(sort(unique(rec$year)), seq(2013, 2025, 3)))
# Same flag as R/24: a couple where either partner reports a previous marriage.
rec[, remarried := MARRIED == 1 & (x7377 == 2 | x7392 == 2)]
# Weighted mid-rank, the same definition used for inc_rank / nw_rank in R/07.
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); r[o] <- (cumsum(w[o]) - w[o] / 2) / sum(w); r }

groups <- c("all", "no_remarried", "first_marriage")
bands <- c("top", "aff", "middle")
labels <- as.vector(outer(groups, bands, paste, sep = "_"))
mom_rows <- c("den", "num3", "numk", "numx3", "num4")
# Weighted moments for each group x band; weights normalized within the wave.
moments <- function(s, w, ranks) {
  w <- w / sum(w)
  out <- matrix(0, length(mom_rows), length(labels), dimnames = list(mom_rows, labels))
  for (j in seq_along(labels)) {
    lab <- labels[j]
    keep <- if (startsWith(lab, "no_remarried")) !s$remarried
            else if (startsWith(lab, "first_marriage")) s$first_marriage_couple %in% TRUE
            else rep(TRUE, nrow(s))
    band <- sub(".*_", "", lab)
    keep <- keep & switch(band, top = ranks > .99, aff = ranks > .9 & ranks <= .99, middle = ranks > .4 & ranks <= .6)
    keep[is.na(keep)] <- FALSE
    k <- s$kids[keep]; wk <- w[keep]
    out[, j] <- c(sum(wk), sum(wk * (k >= 3)), sum(wk * k), sum(wk * (k == 3)), sum(wk * (k >= 4)))
  }
  out
}
statfun <- function(m) {
  val <- c()
  for (g in groups) {
    q <- m[, paste0(g, c("_top", "_aff", "_middle")), drop = FALSE]
    a <- 100 * q["num3", ] / q["den", ]; k <- q["numk", ] / q["den", ]
    x3 <- 100 * q["numx3", ] / q["den", ]; f4 <- 100 * q["num4", ] / q["den", ]
    v <- c(top3 = a[[1]], aff3 = a[[2]], mid3 = a[[3]], gap3 = a[[1]] - a[[2]], gapmid3 = a[[1]] - a[[3]],
           topkids = k[[1]], affkids = k[[2]], gapkids = k[[1]] - k[[2]],
           top_exact3 = x3[[1]], aff_exact3 = x3[[2]], gap_exact3 = x3[[1]] - x3[[2]],
           top_4plus = f4[[1]], aff_4plus = f4[[2]], gap_4plus = f4[[1]] - f4[[2]])
    names(v) <- paste(g, names(v), sep = "_")
    val <- c(val, v)
  }
  val
}

years <- seq(2013, 2025, 3)
results <- list()
for (rk in c("inc_rank", "nw_rank")) {
  v <- if (rk == "inc_rank") "inc_measure" else "NETWORTH"
  per <- list()
  for (yr in years) {
    s <- rec[year == yr]; s1 <- s[implicate == 1]
    rw <- as.data.table(zap_labels(read_dta(sprintf("data_raw/rw/p%02d_rw1.dta", yr %% 100))))
    setnames(rw, tolower(names(rw)))
    ix <- match(s1$Y1, rw$y1)
    if (anyNA(ix)) stop("replicate weights missing for some implicate-1 cases in ", yr)
    imp <- lapply(1:5, function(i) { z <- s[implicate == i]; moments(z, z$WGT, z[[rk]]) })
    draws <- array(0, c(length(mom_rows), length(labels), 999))
    for (b in 1:999) {
      w <- rw[[paste0("wt1b", b)]][ix] * rw[[paste0("mm", b)]][ix]; w[is.na(w)] <- 0
      r <- s1[[rk]]
      for (a in unique(s1$age5)) { j <- which(s1$age5 == a); r[j] <- prank(s1[[v]][j], w[j]) }
      draws[, , b] <- moments(s1, w, r)
    }
    per[[as.character(yr)]] <- list(imp = imp, draws = draws)
    message(rk, ": ", yr, " done (", length(ix), " implicate-1 families)")
  }
  M <- lapply(1:5, function(i) Reduce(`+`, lapply(per, function(z) z$imp[[i]])))
  Q <- sapply(M, statfun); est <- rowMeans(Q); B <- apply(Q, 1, var); base <- M[[1]]
  # Jacobian of the pooled statistics with respect to the moments (central differences).
  grad <- sapply(seq_along(base), function(j) {
    h <- 1e-6; up <- dn <- base; up[j] <- up[j] + h; dn[j] <- dn[j] - h
    (statfun(up) - statfun(dn)) / (2 * h)
  })
  # var() divides by 998 for 999 replicates, the Fed's convention.
  V <- Reduce(`+`, lapply(per, function(z) { D <- matrix(z$draws, nrow = length(base)); apply(grad %*% D, 1, var) }))
  se <- sqrt(V + 1.2 * B)
  results[[rk]] <- data.table(rank = rk, stat = names(est), est = est, se = se, lower = est - 1.96 * se, upper = est + 1.96 * se)
}
out <- rbindlist(results)
attr(out, "built_at") <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
saveRDS(out, "output/kids_rep5.rds")
options(width = 200)
print(out[grepl("gap", stat)], digits = 3)
message("Wrote output/kids_rep5.rds: ", nrow(out), " statistics, pooled 2013-2025, ranks re-estimated in replicates")
