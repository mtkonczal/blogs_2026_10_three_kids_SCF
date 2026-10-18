# Independent process audit of the local SCF inputs and production objects.
# Run from project root. No production estimators are sourced or modified.
suppressPackageStartupMessages({library(data.table); library(haven); library(jsonlite)})
setDTthreads(2)
wave_years <- seq(1989, 2025, 3)
rep_years <- c(1989, 1992, 1995, 1998, 2013, 2016, 2019, 2022, 2025)
linked <- readRDS('data/scf_linked.rds')
main <- readRDS('output/kids_main.rds')
stopifnot(!anyDuplicated(linked[, .(year, Y1)]), !anyDuplicated(main[, .(year, Y1)]),
          !anyDuplicated(linked[, .(year, YY1, implicate)]),
          all(linked$Y1 == 10 * linked$YY1 + linked$implicate),
          setequal(unique(linked$year), wave_years))
rel <- c('x108','x114','x120','x126','x132','x202','x208','x214','x220','x226')
liv <- c('x112','x118','x124','x130','x136','x206','x212','x218','x224','x230')
rank_independent <- function(amount, weights) {
  order_index <- order(amount)
  out <- numeric(length(amount))
  out[order_index] <- (cumsum(weights[order_index]) - weights[order_index] / 2) / sum(weights)
  out
}
joined <- list(); waves <- list(); rep_checks <- list()
fields_checked <- 0L; cells_checked <- 0
for (wv in wave_years) {
  raw_csv <- fread(sprintf('data_raw/xlsx/SCFP%d.csv', wv), integer64 = 'double')
  if ('X1' %in% names(raw_csv)) setnames(raw_csv, c('X1','XX1'), c('Y1','YY1'))
  stopifnot(!anyDuplicated(raw_csv$Y1), all(raw_csv$WGT > 0), all(is.finite(raw_csv$WGT)))
  db <- linked[year == wv]
  ix <- match(raw_csv$Y1, db$Y1)
  stopifnot(nrow(db) == nrow(raw_csv), !anyNA(ix), setequal(raw_csv$Y1, db$Y1))
  stopifnot(all(raw_csv[, .N, by = YY1]$N == 5),
            identical(sort(unique(db$implicate)), 1:5),
            all(raw_csv$Y1 == 10 * raw_csv$YY1 + raw_csv$Y1 %% 10))
  # Audit every retained summary column, allowing only the documented old-income edit.
  retained <- setdiff(names(raw_csv), 'WILSH')
  edits <- if (wv %in% c(1989, 1992)) intersect(retained, c('NINCCAT','NINC2CAT','NINCPCTLECAT','NINCQRTCAT')) else character()
  if (wv %in% c(1989, 1992)) stopifnot(all(is.na(raw_csv$NORMINC)))
  for (field in retained) {
    expected <- if (field %in% edits) rep(NA_real_, nrow(raw_csv)) else raw_csv[[field]]
    actual <- db[[field]][ix]
    stopifnot(length(actual) == length(expected), identical(is.na(actual), is.na(expected)),
              all(actual[!is.na(expected)] == expected[!is.na(expected)]))
    fields_checked <- fields_checked + 1L; cells_checked <- cells_checked + length(expected)
  }
  extra <- setdiff(names(db), c(retained, 'year', 'implicate'))
  stopifnot(all(vapply(extra, function(field) all(is.na(db[[field]])), logical(1))))
  f <- sprintf('data_raw/full/p%02di%d.dta', wv %% 100, if (wv == 1992) 4 else 6)
  nm <- names(read_dta(f, n_max = 1))
  want <- c('y1','x1','x42001','x5910','x7377','x7392','x8023','x105','x19', rel, liv)
  raw <- as.data.table(zap_labels(read_dta(f, col_select = dplyr::all_of(nm[tolower(nm) %in% want]))))
  setnames(raw, tolower(names(raw)))
  if ('x1' %in% names(raw)) setnames(raw, 'x1', 'y1')
  absent_roster_fields <- setdiff(c(rel, liv), names(raw))
  # Historical public releases omit some roster columns. Mark them explicitly;
  # the check below still requires the reconstructed count to equal official KIDS.
  for (field in absent_roster_fields) raw[, (field) := NA_real_]
  stopifnot(!anyDuplicated(raw$y1), nrow(raw) == nrow(raw_csv), setequal(raw$y1, raw_csv$Y1))
  raw <- raw[match(raw_csv$Y1, y1)]
  stopifnot(all(raw$y1 == raw_csv$Y1), !anyNA(raw$x5910), all(raw$x5910 >= -1))
  weight_error <- max(abs(5 * raw_csv$WGT / raw$x42001 - 1))
  stopifnot(weight_error < 1e-8)
  roster_matrix <- as.matrix(raw[, ..rel]); residence_matrix <- as.matrix(raw[, ..liv])
  # Count observed child codes, matching the Fed construction; retain missing-slot diagnostics.
  roster_children <- rowSums(roster_matrix == 4 | roster_matrix == 13 | roster_matrix == 36, na.rm = TRUE)
  roster_away <- rowSums((roster_matrix == 4 | roster_matrix == 13 | roster_matrix == 36) & residence_matrix == 5, na.rm = TRUE)
  stopifnot(!anyNA(roster_children), !anyNA(roster_away), all(roster_children == raw_csv$KIDS))
  keep <- raw_csv$AGE >= 45 & raw_csv$AGE <= 64
  z <- raw_csv[keep, .(year = wv, Y1, YY1, AGE, KIDS, WGT, INCOME, NORMINC = as.numeric(NORMINC), NETWORTH, MARRIED)]
  z[, `:=`(implicate = Y1 %% 10, age5 = floor(AGE / 5) * 5,
           kids_away = pmax(raw$x5910[keep], 0), away_roster = roster_away[keep])]
  z[, `:=`(kids = KIDS + kids_away, inc_measure = fifelse(is.na(NORMINC), INCOME, NORMINC))]
  if (wv >= 2013) {
    z[, `:=`(previous_r = raw$x7377[keep], previous_s = raw$x7392[keep],
             marital_r = raw$x8023[keep], marital_s = raw$x105[keep], spouse_age = raw$x19[keep])]
  } else {
    z[, c('previous_r','previous_s','marital_r','marital_s','spouse_age') := NA_real_]
  }
  setorder(z, Y1)
  z[, inc_rank := rank_independent(inc_measure, WGT), by = .(implicate, age5)]
  z[, nw_rank := rank_independent(NETWORTH, WGT), by = .(implicate, age5)]
  z[, w := WGT / sum(WGT), by = implicate]
  stopifnot(all(is.finite(z$inc_rank)), all(is.finite(z$nw_rank)),
            all(z$inc_rank > 0 & z$inc_rank < 1), all(z$nw_rank > 0 & z$nw_rank < 1),
            all(abs(z[, sum(w), by = implicate]$V1 - 1) < 1e-12))
  p <- main[year == wv]; j <- match(z$Y1, p$Y1)
  stopifnot(nrow(z) == nrow(p), !anyNA(j), setequal(z$Y1, p$Y1))
  verify <- c('AGE','KIDS','WGT','INCOME','NORMINC','NETWORTH','MARRIED','implicate','age5','kids_away','kids','inc_measure','inc_rank','nw_rank','w')
  for (field in verify) {
    expected <- z[[field]]; actual <- p[[field]][j]
    stopifnot(identical(is.na(actual), is.na(expected)), all(abs(actual - expected) < 1e-12, na.rm = TRUE))
  }
  # Explicit boundary rule, independently converted to the 12 display-bin numbers.
  cuts <- c(seq(.1, .9, .1), .95, .99)
  for (rk in c('inc_rank','nw_rank')) {
    expected_bin <- 1L + rowSums(outer(z[[rk]], cuts, `>`))
    actual_bin <- as.integer(p[[if (rk == 'inc_rank') 'inc_bin' else 'nw_bin']][j])
    stopifnot(all(expected_bin == actual_bin))
  }
  waves[[as.character(wv)]] <- data.table(year = wv, summary_rows = nrow(db), summary_fields = length(retained),
    unique_families = uniqueN(db$YY1), merge_missing = 0L, merge_duplicates = 0L,
    max_weight_ratio_error = weight_error, roster_disagreements = sum(roster_children != raw_csv$KIDS),
    missing_roster_relationship_cells = sum(is.na(roster_matrix)),
    absent_roster_fields = paste(absent_roster_fields, collapse = ", "),
    missing_roster_residence_cells = sum(is.na(residence_matrix)),
    eligible_rows = nrow(z), eligible_cases = uniqueN(z$YY1), sample_rank_weight_disagreements = 0L,
    usual_income_unavailable = all(is.na(raw_csv$NORMINC)), population_millions = sum(raw_csv$WGT) / 1e6)
  joined[[as.character(wv)]] <- z
  if (wv %in% rep_years) {
    rw <- as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta', wv %% 100))))
    setnames(rw, tolower(names(rw)))
    if ('x1' %in% names(rw)) setnames(rw, 'x1', 'y1')
    stopifnot(!anyDuplicated(rw$y1), all(rw$y1 %% 10 == 1))
    rw_names <- c(paste0('wt1b', 1:999), paste0('mm', 1:999))
    stopifnot(all(rw_names %in% names(rw)))
    ids <- db[implicate == 1, Y1]; jrw <- match(ids, rw$y1)
    stopifnot(!anyNA(jrw))
    W <- vapply(1:999, function(b) {
      v <- rw[[paste0('wt1b', b)]][jrw] * rw[[paste0('mm', b)]][jrw]
      v[is.na(v)] <- 0; v
    }, numeric(length(ids)))
    stopifnot(all(is.finite(W)), all(W >= 0), all(colSums(W) > 0))
    # Check every bootstrap denominator in each age band, not only total-wave weight.
    age <- db$AGE[match(ids, db$Y1)]; band <- floor(age / 5) * 5
    for (a in c(45, 50, 55, 60)) stopifnot(all(colSums(W[age >= 45 & age <= 64 & band == a, , drop = FALSE]) > 0))
    rep_checks[[as.character(wv)]] <- data.table(year = wv, replicate_columns = ncol(W), first_implicate_ids = length(ids),
       missing_weight_ids = 0L, negative_or_nonfinite_weights = 0L, empty_age_band_replicates = 0L)
  }
  message(wv, ': all retained fields, one-to-one joins, children, weights, ranks and ', if (wv %in% rep_years) '999 replicate weights checked' else 'sample checked')
}
d <- rbindlist(joined)
# Reconstruct headline ratios and MI aggregation directly, without production helpers.
head <- readRDS('output/kids_rep5.rds')
checks <- list()
for (rk in c('inc_rank','nw_rank')) {
  per_imp <- rbindlist(lapply(1:5, function(m) {
    z <- d[year >= 2013 & implicate == m]
    t <- z[get(rk) > .99]; a <- z[get(rk) > .9 & get(rk) <= .99]
    wm <- function(x, w) sum(x * w) / sum(w)
    data.table(implicate = m, top3 = 100 * wm(t$kids >= 3, t$w), aff3 = 100 * wm(a$kids >= 3, a$w),
      topkids = wm(t$kids, t$w), affkids = wm(a$kids, a$w),
      top_exact3 = 100 * wm(t$kids == 3, t$w), aff_exact3 = 100 * wm(a$kids == 3, a$w),
      top_4plus = 100 * wm(t$kids >= 4, t$w), aff_4plus = 100 * wm(a$kids >= 4, a$w))
  }))
  per_imp[, `:=`(gap3 = top3 - aff3, gapkids = topkids - affkids,
                gap_exact3 = top_exact3 - aff_exact3, gap_4plus = top_4plus - aff_4plus)]
  for (st in setdiff(names(per_imp), 'implicate')) {
    actual <- head[rank == rk & stat == paste0('all_', st), est]
    stopifnot(length(actual) == 1L)
    err <- abs(mean(per_imp[[st]]) - actual)
    stopifnot(err < 1e-9)
    checks[[paste(rk, st)]] <- data.table(rank = rk, stat = st, est = mean(per_imp[[st]]), production = actual, absolute_error = err)
  }
}
# Independently check the raw flags and all counting-sensitivity point estimates,
# including combined restrictions, using grouped weighted means rather than R/29 moments.
count_prod <- readRDS('output/kids_count_checks.rds')
recent <- d[year >= 2013]
recent[, strict := MARRIED == 1 & marital_r == 1 & marital_s == 1 & previous_r == 1 & previous_s == 1]
count_checks <- list()
for (rk in c('inc_rank','nw_rank')) for (specification in unique(count_prod$spec)) {
  z <- copy(recent)
  if (specification %in% c('strict_first','strict_first_overlap_all','strict_first_overlap_top')) z <- z[strict %in% TRUE]
  if (specification == 'older') z <- z[AGE >= 50 & (MARRIED == 2 | spouse_age >= 45)]
  z[, outcome := kids]
  if (specification %in% c('overlap_all','strict_first_overlap_all')) z[, outcome := kids - pmin(away_roster, kids_away)]
  if (specification %in% c('overlap_top','strict_first_overlap_top')) z[get(rk) > .99, outcome := kids - pmin(away_roster, kids_away)]
  q <- z[get(rk) > .9, .(share = 100 * weighted.mean(outcome >= 3, w)), by = .(implicate, top = get(rk) > .99)]
  stopifnot(nrow(q) == 10L)
  top <- mean(q[top == TRUE, share]); aff <- mean(q[top == FALSE, share])
  prod <- count_prod[rank == rk & spec == specification]
  err <- max(abs(c(top - prod$top, aff - prod$aff, top - aff - prod$gap)))
  stopifnot(nrow(prod) == 1L, err < 1e-9)
  count_checks[[paste(rk, specification)]] <- data.table(rank = rk, spec = specification, top, aff, gap = top - aff, absolute_error = err)
}
# Record the finite sample effect of using pooled rows rather than average ratios.
mi_difference <- rbindlist(lapply(c('inc_rank','nw_rank'), function(rk) {
  z <- d[year >= 2013]
  z[, bin := 1L + rowSums(outer(get(rk), c(seq(.1,.9,.1),.95,.99), `>`))]
  pooled <- z[, .(pooled = 100 * weighted.mean(kids >= 3, w)), by = bin]
  averaged <- z[, .(share = 100 * weighted.mean(kids >= 3, w)), by = .(implicate, bin)][, .(mi_mean = mean(share)), by = bin]
  merge(pooled, averaged, by = 'bin')[, `:=`(rank = rk, difference = pooled - mi_mean)]
}))
manifest <- fread('data_raw/download_manifest.csv')
stopifnot(all(unname(tools::md5sum(manifest$zip)) == manifest$zip_md5))
files <- c('blog_post_text.md', 'data/scf_linked.rds', 'output/kids_main.rds',
           sprintf('data_raw/xlsx/SCFP%d.csv', wave_years),
           sprintf('data_raw/full/p%02di%d.dta', wave_years %% 100, ifelse(wave_years == 1992, 4, 6)),
           sprintf('data_raw/rw/p%02d_rw1.dta', rep_years %% 100), list.files('R', full.names = TRUE, pattern = '\\.R$'))
hashes <- data.table(path = files, md5 = unname(tools::md5sum(files)))
stopifnot(!anyNA(hashes$md5))
summary <- list(review_date = '2026-10-16', summary_rows = nrow(linked), summary_columns = ncol(linked),
  checked_wave_columns = fields_checked, checked_summary_cells = cells_checked,
  waves = rbindlist(waves), replicate_weights = rbindlist(rep_checks), headlines = rbindlist(checks),
  counting = rbindlist(count_checks), mi_pooling = mi_difference, max_pooling_difference_pp = max(abs(mi_difference$difference)),
  input_and_script_hashes = hashes)
fwrite(summary$waves, 'audit/results/process_factcheck_20261016_waves.csv')
fwrite(summary$replicate_weights, 'audit/results/process_factcheck_20261016_replicates.csv')
fwrite(summary$headlines, 'audit/results/process_factcheck_20261016_headlines.csv')
fwrite(summary$counting, 'audit/results/process_factcheck_20261016_counting.csv')
write_json(summary, 'audit/results/process_factcheck_20261016.json', dataframe = 'rows', auto_unbox = TRUE, digits = 15, pretty = TRUE)
print(summary$waves)
print(summary$replicate_weights)
message('Process audit passed: ', fields_checked, ' wave-columns, ', cells_checked, ' summary cells; max pooled-vs-MI difference ', signif(summary$max_pooling_difference_pp, 5), ' pp.')
