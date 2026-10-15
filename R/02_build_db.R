# 02_build_db.R
# Stack the 13 SCF summary extracts (1989-2025) into one harmonized, linked database.
#
# "Linked" here means a pooled repeated cross-section on a common schema, keyed by
# (year, case_id, implicate). The SCF is NOT a panel (the 2007-09 re-interview panel is
# a separate file), so the same case_id in different years is a different family.
#
# Harmonization decisions (documented because they matter for interpretation):
#   * 1989 names the IDs X1/XX1; later waves use Y1/YY1. Renamed to Y1/YY1.
#   * implicate = Y1 - 10*YY1 (1..5). Each family appears 5 times (multiple imputation).
#   * WGT in the summary extract is already divided by 5, so sum(WGT) over all
#     implicates = number of families. We keep WGT as-is.
#   * Dollar values: every extract in this vintage is already in 2025 dollars
#     (Fed re-releases history with each wave). We do NOT re-deflate. (Checked earlier
#     against the October 2026 Fed Bulletin's 2022 column, also in 2025 dollars.)
#   * Columns blank in a wave (e.g. NORMINC in 1989, before usual income was asked)
#     are NA. Many questions not asked in a wave are coded 0 rather than NA in the
#     Fed extract, so check that a variable actually varies in a wave before using it.
#   * Variables introduced after 1989 are NA in waves where they do not exist.
#     Caution: a variable existing in a wave does not guarantee a constant
#     question wording; see the codebooks in docs/ before using newer survey items
#     in long time series.
#
# Output:
#   data/scf_linked.rds      one data.table, one row per family x implicate

suppressPackageStartupMessages(library(data.table))

root    <- getwd()
csv_dir <- file.path(root, "data_raw", "xlsx")
out_dir <- file.path(root, "data")
dir.create(out_dir, showWarnings = FALSE)
years   <- seq(1989, 2025, by = 3)

read_wave <- function(y) {
  # integer64 = "double": 2025 has values > 2^31 (ASSET, NFIN, NONACTBUS); integer64
  # columns do not stack safely with doubles from other waves.
  d <- fread(file.path(csv_dir, sprintf("SCFP%d.csv", y)), showProgress = FALSE,
             integer64 = "double")
  if ("X1" %in% names(d)) setnames(d, c("X1", "XX1"), c("Y1", "YY1"))
  # Columns that are entirely blank in a wave come in as logical NA; make them numeric
  for (v in names(d)) if (is.logical(d[[v]])) set(d, j = v, value = as.numeric(d[[v]]))
  # WILSH (Wilshire index placeholder) is a non-numeric code in the extracts; drop it
  if ("WILSH" %in% names(d)) d[, WILSH := NULL]
  # Usual income (NORMINC) was first asked in 1995. In the 1989 and 1992 extracts the
  # usual-income category variables are populated anyway but are uncorrelated with
  # income (rank correlation ~ -0.03), i.e. not meaningful. Blank them so they cannot be
  # used by accident. This is the only value-level edit made to the Fed's data.
  if (all(is.na(d$NORMINC))) {
    for (v in intersect(c("NINCCAT", "NINC2CAT", "NINCPCTLECAT", "NINCQRTCAT"), names(d)))
      set(d, j = v, value = NA_real_)
    message(y, ": NORMINC not asked; set NINC* usual-income categories to NA")
  }
  stopifnot(all(c("Y1", "YY1", "WGT", "NETWORTH", "INCOME") %in% names(d)))
  d[, `:=`(year = y, implicate = as.integer(Y1 - 10L * YY1))]
  # Diagnostics: fail loudly if structure is unexpected
  imps <- sort(unique(d$implicate))
  if (!identical(imps, 1:5)) stop(y, ": implicates are ", paste(imps, collapse = ","))
  n_by_imp <- d[, .N, by = implicate]$N
  if (length(unique(n_by_imp)) != 1) stop(y, ": unequal rows per implicate")
  if (anyDuplicated(d$Y1)) stop(y, ": duplicate Y1")
  message(sprintf("%d: %6d rows, %5d families x 5 implicates, %3d vars, %.1fM families (sum WGT)",
                  y, nrow(d), n_by_imp[1], ncol(d) - 2, sum(d$WGT) / 1e6))
  d
}

waves <- lapply(years, read_wave)
names(waves) <- years

scf <- rbindlist(waves, use.names = TRUE, fill = TRUE)
setcolorder(scf, c("year", "YY1", "Y1", "implicate", "WGT"))
setkey(scf, year, YY1, implicate)

# Sanity checks on the stacked file
chk <- scf[, .(families_M = sum(WGT) / 1e6,
               n_cases = uniqueN(YY1),
               med_nw_imp1 = { i <- implicate == 1; x <- NETWORTH[i]; w <- WGT[i]; o <- order(x)
                               x[o][which(cumsum(w[o]) >= sum(w) / 2)[1]] }),
           by = year]
print(chk)
stopifnot(nrow(scf) == sum(sapply(waves, nrow)))

saveRDS(scf, file.path(out_dir, "scf_linked.rds"), compress = "xz")
message(sprintf("Wrote scf_linked.rds (%.0f MB): %d rows x %d cols",
                file.size(file.path(out_dir, "scf_linked.rds")) / 1e6, nrow(scf), ncol(scf)))
