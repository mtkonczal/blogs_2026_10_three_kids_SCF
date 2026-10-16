# Summary-extract-only sanity check; no full .dta files, analysis .rds files,
# or replicate weights are read. Run from the repository root:
# Rscript audit/summary_csv_check_20261016.R
#
# Primary comparison: heads 45-64, pooled 2013-2025, production age/year ranks
# and equal weight per wave. Change only the outcome to summary-extract KIDS.
# KIDS counts roster children, including children-in-law and some dependents
# away from home. It does NOT add the full survey's X5910 children-elsewhere count.
# This is a different outcome, not an alternative estimate of total children.
# The two younger age windows are supplementary checks, not specification changes.
# Point estimates only: no sampling significance is calculated here.

suppressPackageStartupMessages(library(data.table))

years <- seq(2013, 2025, by = 3)
cols <- c("YY1", "Y1", "WGT", "AGE", "KIDS", "NORMINC", "NETWORTH")
d <- rbindlist(lapply(years, function(yr) {
  fread(sprintf("data_raw/xlsx/SCFP%d.csv", yr), select = cols)[, year := yr]
}))
d[, implicate := as.integer(Y1 - 10 * YY1)]
setorder(d, year, Y1)
stopifnot(!anyNA(d), all(d$WGT > 0), all(d$implicate %in% 1:5),
          !anyDuplicated(d[, .(year, Y1)]))

# Same weighted midpoint ranks and deterministic tie order as production.
prank <- function(x, w) {
  o <- order(x)
  r <- numeric(length(x))
  r[o] <- (cumsum(w[o]) - w[o] / 2) / sum(w)
  r
}

results <- list()
for (ages in list(c(45, 64), c(35, 44), c(40, 49))) {
  s <- copy(d[AGE >= ages[1] & AGE <= ages[2]])
  s[, age5 := floor(AGE / 5) * 5]
  s[, w := WGT / sum(WGT), by = .(year, implicate)]

  for (v in c("NORMINC", "NETWORTH")) {
    s[, rank_value := prank(get(v), WGT), by = .(year, implicate, age5)]
    s[, band := fcase(
      rank_value > .99, "Top 1%",
      rank_value > .9, "90th-99th",
      rank_value > .4 & rank_value <= .6, "40th-60th",
      default = "Other"
    )]
    # Average implicate-specific pooled ratios, as in production inference.
    per <- s[band != "Other", .(
      share3 = 100 * sum(w * (KIDS >= 3)) / sum(w),
      exact3 = 100 * sum(w * (KIDS == 3)) / sum(w),
      mean_kids = sum(w * KIDS) / sum(w)
    ), by = .(implicate, band)]
    z <- per[, lapply(.SD, mean), by = band,
             .SDcols = c("share3", "exact3", "mean_kids")]
    top <- z[band == "Top 1%"]
    aff <- z[band == "90th-99th"]
    z <- rbind(z, data.table(
      band = "Top 1% minus 90th-99th",
      share3 = top$share3 - aff$share3,
      exact3 = top$exact3 - aff$exact3,
      mean_kids = top$mean_kids - aff$mean_kids
    ))
    z[, `:=`(sample = paste(ages, collapse = "-"), rank = v)]
    results[[paste(c(ages, v), collapse = "_")]] <- z
  }
}

out <- rbindlist(results)
stopifnot(nrow(out) == 24, !anyNA(out))
dir.create("audit/results", recursive = TRUE, showWarnings = FALSE)
fwrite(out, "audit/results/summary_csv_check_20261016.csv")
options(width = 160)
print(out, digits = 5)
