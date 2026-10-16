# 29_family_count_checks.R
# Counting and family-history sensitivities for the headline gap (share with 3+ children,
# top 1% vs. 90th-99th), heads 45-64, pooled 2013-2025. Supports footnote 2 of the post.
# Estimator as in R/25: ranks within year/implicate/age band, re-estimated in every replicate
# (999 Fed replicate weights, implicate 1); equal weight per wave; independent waves; 1.2 B.
# Ranks always come from the full sample, so "top 1%" keeps its baseline meaning; each
# restriction changes only which families enter the comparison.
#
# Specifications:
#  - overlap_all: subtract the maximum possible double count, min(A, B), from every family, where
#    A = roster children who do not usually live there (relationship code 4/13/36 with residence
#    code 5 on X112-type items) and B = children living elsewhere (X5910).
#  - overlap_top: the adverse case, subtracting that overlap only in the top 1%.
#  - strict_first: currently married couples (X8023 = 1 and X105 = 1) where both report a first
#    marriage (X7377 = 1 and X7392 = 1). Unlike R/24's flag, this excludes cohabiters.
#  - older: head 50-64, and a spouse/partner, if present, aged at least 45 (X19).
#  - strict_first_overlap_all / _top: the first-marriage restriction combined with each overlap correction.
# Baseline must reproduce R/25; the separate specifications reproduce the independent audit
# (audit/review_20261015/results/independent_inference.csv).
source("R/scf_functions.R")
suppressPackageStartupMessages({ library(haven) })

years <- seq(2013, 2025, 3)
main <- readRDS("output/kids_main.rds")[year %in% years,
  .(year, Y1, implicate, WGT, AGE, age5, MARRIED, kids, kids_away, x7377, x7392, inc_measure, NETWORTH)]
rel <- c("x108", "x114", "x120", "x126", "x132", "x202", "x208", "x214", "x220", "x226")
liv <- c("x112", "x118", "x124", "x130", "x136", "x206", "x212", "x218", "x224", "x230")
fx <- rbindlist(lapply(years, function(wv) {
  f <- sprintf("data_raw/full/p%02di6.dta", wv %% 100); nm <- names(read_dta(f, n_max = 1))
  z <- as.data.table(zap_labels(read_dta(f, col_select = dplyr::all_of(nm[tolower(nm) %in% c("y1", "x8023", "x105", "x19", rel, liv)]))))
  setnames(z, tolower(names(z)))
  R <- as.matrix(z[, ..rel]); L <- as.matrix(z[, ..liv])
  data.table(year = wv, Y1 = z$y1, marital_r = z$x8023, marital_s = z$x105, spouse_age = z$x19,
             away_roster = rowSums((R == 4 | R == 13 | R == 36) & L == 5, na.rm = TRUE))
}))
d <- merge(main, fx, by = c("year", "Y1"))
stopifnot(nrow(d) == nrow(main))
setorder(d, year, Y1)   # same tie order as R/07 and R/25
d[, kids_min := kids - pmin(away_roster, kids_away)]
d[, strict_first := MARRIED == 1 & marital_r == 1 & marital_s == 1 & x7377 == 1 & x7392 == 1]
d[, older := AGE >= 50 & (MARRIED == 2 | spouse_age >= 45)]

specs <- c("baseline", "overlap_all", "overlap_top", "strict_first", "older", "strict_first_overlap_all", "strict_first_overlap_top")
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); r[o] <- (cumsum(w[o]) - w[o] / 2) / sum(w); r }
rank_age <- function(x, w, age5) { r <- numeric(length(x)); for (a in unique(age5)) { j <- which(age5 == a); r[j] <- prank(x[j], w[j]) }; r }
sel <- function(s, nm, r) {
  k <- s$kids; keep <- rep(TRUE, nrow(s))
  if (startsWith(nm, "strict_first")) keep <- s$strict_first %in% TRUE
  if (nm == "older") keep <- s$older %in% TRUE
  if (endsWith(nm, "overlap_all")) k <- s$kids_min
  if (endsWith(nm, "overlap_top")) k <- ifelse(r > .99, s$kids_min, s$kids)
  list(keep = keep, y = 100 * (k >= 3))
}
# Moments (den_top, num_top, den_aff, num_aff) per spec; weights normalized within the wave.
mom <- function(s, w, r) { w <- w / sum(w); sapply(specs, function(nm) { a <- sel(s, nm, r)
  t <- a$keep & r > .99; f <- a$keep & r > .9 & r <= .99
  c(sum(w[t]), sum(w[t] * a$y[t]), sum(w[f]), sum(w[f] * a$y[f])) }) }

out <- list()
for (rk in c("inc_rank", "nw_rank")) {
  v <- if (rk == "inc_rank") "inc_measure" else "NETWORTH"
  base <- lapply(1:5, function(i) Reduce(`+`, lapply(years, function(wv) {
    s <- d[year == wv & implicate == i]; mom(s, s$WGT, rank_age(s[[v]], s$WGT, s$age5)) })))
  Q <- sapply(base, function(m) m[2, ] / m[1, ] - m[4, ] / m[3, ]); b <- base[[1]]
  g <- rbind(-b[2, ] / b[1, ]^2, 1 / b[1, ], b[4, ] / b[3, ]^2, -1 / b[3, ]); V <- 0
  for (wv in years) {
    s <- d[year == wv & implicate == 1]
    rw <- as.data.table(zap_labels(read_dta(sprintf("data_raw/rw/p%02d_rw1.dta", wv %% 100)))); setnames(rw, tolower(names(rw)))
    ix <- match(s$Y1, rw$y1); if (anyNA(ix)) stop("replicate weights missing for some implicate-1 cases in ", wv)
    D <- t(sapply(1:999, function(j) { w <- rw[[paste0("wt1b", j)]][ix] * rw[[paste0("mm", j)]][ix]; w[is.na(w)] <- 0
      colSums(g * mom(s, w, rank_age(s[[v]], w, s$age5))) }))
    V <- V + apply(D, 2, var)   # var() divides by 998, the Fed's convention
  }
  est <- rowMeans(Q); se <- sqrt(V + 1.2 * apply(Q, 1, var))
  out[[rk]] <- data.table(rank = rk, spec = specs, top = rowMeans(sapply(base, function(m) m[2, ] / m[1, ])),
                          aff = rowMeans(sapply(base, function(m) m[4, ] / m[3, ])), gap = est, se = se,
                          lower = est - 1.96 * se, upper = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))
}
res <- rbindlist(out)

# Checks: baseline reproduces R/25; separate specs reproduce the independent audit.
r25 <- readRDS("output/kids_rep5.rds")[stat == "all_gap3"]
stopifnot(all(abs(res[spec == "baseline"][order(rank), gap] - r25[order(rank), est]) < 1e-6),
          all(abs(res[spec == "baseline"][order(rank), se] - r25[order(rank), se]) < 1e-3))
aud <- fread("audit/review_20261015/results/independent_inference.csv")
map <- c(overlap_all = "max_overlap_all", overlap_top = "max_overlap_top_only", strict_first = "strict_current_first_marriage", older = "older_spouses")
cmp <- merge(res[spec %in% names(map)][, .(rank = fifelse(rank == "inc_rank", "ir", "nr"), spec = map[spec], gap, se)],
             aud[, .(rank, spec, gap_a = gap / 1, se_a = se)], by = c("rank", "spec"))
print(cmp)
stopifnot(nrow(cmp) == 8, cmp[, all(abs(gap - gap_a) < 1e-6 & abs(se - se_a) < 1e-3)])

attr(res, "built_at") <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
saveRDS(res, "output/kids_count_checks.rds")
options(width = 200)
print(res, digits = 3)
message("Wrote output/kids_count_checks.rds")
