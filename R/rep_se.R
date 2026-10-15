# rep_se.R: standard errors from the Fed's SCF replicate weights + multiple imputation.
#
# Replicate weights (data_raw/rw/pYY_rw1.dta, from scf{YEAR}rw1s.zip) are 999 bootstrap
# replicates for implicate-1 cases: replicate r weight = wt1b{r} * mm{r} (missing -> 0).
# Total variance = sampling variance + (1 + 1/5) * between-implicate variance, where the
# sampling variance is computed on implicate 1 across replicates with scale 1/998
# (the convention used in standard SCF replicate-weight survey designs).
# Point estimates match the rest of this project (pooled implicates, weight WGT).
suppressPackageStartupMessages({ library(data.table); library(haven) })
.rw_cache <- new.env()
load_reps <- function(year) {
  key <- as.character(year)
  if (!is.null(.rw_cache[[key]])) return(.rw_cache[[key]])
  f <- sprintf("data_raw/rw/p%02d_rw1.dta", year %% 100)
  r <- as.data.table(zap_labels(read_dta(f)))
  R <- 999
  m <- matrix(0, nrow(r), R)
  for (i in seq_len(R)) { w <- r[[paste0("wt1b", i)]] * r[[paste0("mm", i)]]; w[is.na(w)] <- 0; m[, i] <- w }
  out <- list(y1 = r$y1, W = m)
  assign(key, out, envir = .rw_cache)
  out
}
# d: one survey year, all 5 implicates (needs Y1, implicate, WGT). fun(s, w) -> named numeric vector.
# Returns est (pooled), se, and the replicate draws (for ratios across years).
rep_stat <- function(d, fun, year = d$year[1]) {
  est <- fun(d, d$WGT)
  imp <- sapply(1:5, function(i) { s <- d[implicate == i]; fun(s, s$WGT) })
  imp <- matrix(imp, nrow = length(est))
  B <- apply(imp, 1, var)
  rw <- load_reps(year)
  s1 <- d[implicate == 1]
  idx <- match(s1$Y1, rw$y1)
  if (anyNA(idx)) stop("replicate weights missing for some implicate-1 cases")
  draws <- sapply(seq_len(ncol(rw$W)), function(r) fun(s1, rw$W[idx, r]))
  draws <- matrix(draws, nrow = length(est))
  Vs <- apply(draws, 1, function(x) sum((x - mean(x))^2) / 998)
  list(est = est, se = sqrt(Vs + 1.2 * B), draws = draws, imp = imp)
}
# Percent change between two rep_stat results (independent cross-sections)
rep_pct_change <- function(a, b) {
  est <- 100 * (b$est / a$est - 1)
  dr <- 100 * (b$draws / a$draws - 1)
  im <- 100 * (b$imp / a$imp - 1)
  Vs <- apply(dr, 1, function(x) sum((x - mean(x))^2) / 998)
  B <- apply(im, 1, var)
  list(est = est, se = sqrt(Vs + 1.2 * B))
}
