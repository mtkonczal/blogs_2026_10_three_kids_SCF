# scf_functions.R
# Shared estimators for the SCF summary extract.
#
# Multiple imputation: each family appears 5 times (implicates 1-5), and WGT is already
# divided by 5. Point estimates are computed on the POOLED file (all 5 implicates,
# weight WGT). Means are identical to averaging implicate-level estimates; for medians
# pooling reproduces the bulletin more closely (mean abs. error 0.38% vs 0.75% across
# the 56 median cells of bulletin Tables 1-2), so it appears to be the Fed's convention.
# We do NOT compute sampling standard errors here: that needs the separate
# replicate-weight file, which is not part of the extract. mi_est_spread() reports
# the between-implicate range as a partial measure of imputation uncertainty.

suppressPackageStartupMessages(library(data.table))

# Weighted quantile: smallest x such that cumulative weight share >= p.
wq <- function(x, w, p = 0.5) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(NA_real_)
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  x[which(cw >= p - 1e-12)[1]]
}
wmean <- function(x, w) { ok <- !is.na(x); sum(x[ok] * w[ok]) / sum(w[ok]) }

# Apply fun(.SD) to the pooled implicates. fun returns a named list of scalars.
mi_est <- function(d, fun, by = NULL) d[, fun(.SD), by = by]

# Implicate-by-implicate version: mean across implicates plus min/max (imputation spread)
mi_est_spread <- function(d, fun, by = NULL) {
  per_imp <- d[, fun(.SD), by = c("implicate", by)]
  stat_cols <- setdiff(names(per_imp), c("implicate", by))
  m  <- per_imp[, lapply(.SD, mean), by = by, .SDcols = stat_cols]
  lo <- per_imp[, lapply(.SD, min),  by = by, .SDcols = stat_cols]
  hi <- per_imp[, lapply(.SD, max),  by = by, .SDcols = stat_cols]
  setnames(lo, stat_cols, paste0(stat_cols, "_lo")); setnames(hi, stat_cols, paste0(stat_cols, "_hi"))
  if (is.null(by)) cbind(m, lo, hi) else m[lo, on = by][hi, on = by]
}

# Share of a total held by families in percentile bands of a ranking variable,
# computed within implicate. Bands are defined on the ranking variable's weighted CDF.
# Ties at band edges are split by sort order (immaterial for continuous wealth).
top_shares <- function(rank_var, value_var, w, cuts = c(0, .5, .9, .99, 1)) {
  o  <- order(rank_var); v <- value_var[o]; ww <- w[o]
  cw <- cumsum(ww) / sum(ww)
  band <- cut(cw, cuts, include.lowest = TRUE, labels = FALSE)
  tot <- sum(v * ww)
  vapply(seq_len(length(cuts) - 1), function(b) sum((v * ww)[band == b]) / tot, numeric(1))
}

# Labels for standard SCF groupings (bulletin definitions)
label_groups <- function(d) {
  d[, age_grp := factor(AGECL, 1:6, c("Less than 35", "35-44", "45-54", "55-64", "65-74", "75 or more"))]
  d[, educ_grp := factor(EDCL, 1:4, c("No high school diploma", "High school diploma", "Some college", "College degree"))]
  # RACECL5 (respondent's race/ethnicity): consistent 5-way coding exists in recent waves;
  # in older waves Asian is not separately identified, so RACECL4 is the long-run variable.
  d[, race4 := factor(RACECL4, 1:4, c("White non-Hispanic", "Black non-Hispanic", "Hispanic or Latino", "Other or Multiple Race"))]
  d[, house_grp := factor(HOUSECL, 1:2, c("Owner", "Renter or other"))]
  d[, ninc_grp := factor(NINCCAT, 1:6, c("Less than 20", "20-39.9", "40-59.9", "60-79.9", "80-89.9", "90-100"))]
  d[, nw_grp := factor(NWCAT, 1:5, c("Less than 25", "25-49.9", "50-74.9", "75-89.9", "90-100"))]
  invisible(d)
}
