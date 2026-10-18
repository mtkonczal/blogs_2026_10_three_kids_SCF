# Evidence for factcheck.html. Run from the project root after the main pipeline
# and audit/review_20261015/independent_checks.R. No empirical specifications change.
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
setDTthreads(2)
results <- list()
record <- function(check, error, tolerance) {
  stopifnot(length(error) == 1L, is.finite(error), error <= tolerance)
  results[[length(results) + 1L]] <<- data.table(check, maximum_absolute_error = error, tolerance)
}
r <- readRDS('output/kids_rep5.rds')
a <- fread('audit/review_20261015/results/independent_inference.csv')
map <- c(all_gap3 = 'baseline', all_gap_exact3 = 'exactly3', all_gap_4plus = 'fourplus', all_gapkids = 'mean_kids')
for (rk in c('inc_rank', 'nw_rank')) for (st in names(map)) {
  p <- r[rank == rk & stat == st]
  q <- a[rank == ifelse(rk == 'inc_rank', 'ir', 'nr') & spec == map[[st]]]
  stopifnot(nrow(p) == 1L, nrow(q) == 1L)
  record(paste(rk, st, 'independent estimate'), abs(p$est - q$gap), 1e-8)
  record(paste(rk, st, 'independent SE'), abs(p$se - q$se), 1e-6)
}
# Direct reconstruction from the 2025 summary CSV, without production helpers.
s <- fread('data_raw/xlsx/SCFP2025.csv', integer64 = 'double')
s <- s[order(NETWORTH, Y1)]
s[, cum_weight := cumsum(WGT) / sum(WGT)]
shares <- c(bottom50 = 100 * s[cum_weight <= .5, sum(EQUITY * WGT)] / s[, sum(EQUITY * WGT)],
            top10 = 100 * s[cum_weight > .9, sum(EQUITY * WGT)] / s[, sum(EQUITY * WGT)])
n <- fread('graphics/post_numbers.csv')
record('Stock share: bottom half', abs(shares[['bottom50']] - n[item == 'equity_share Ranked by wealth Bottom 50%', value]), 1e-8)
record('Stock share: top tenth', abs(shares[['top10']] - n[item == 'equity_share Ranked by wealth Top 10%', value]), 1e-8)
cc <- readRDS('output/kids_count_checks.rds')
td <- readRDS('output/kids_top1_defs.rds')
pc <- readRDS('output/kids_period_change.rds')
stopifnot(nrow(td) == 15L, all(td$lower > 0), nrow(cc) == 14L, all(cc$lower > 0))
# Validate the footnote ranges, including their whole-percentage-point rounding.
stopifnot(identical(round(range(td[rank == 'income', gap])), c(7, 15)),
          identical(round(range(td[rank == 'wealth', gap])), c(9, 16)))
# Do not call these source hashes evidence that a fresh network download occurred.
manifest <- fread('data_raw/download_manifest.csv')
actual_hash <- unname(tools::md5sum(manifest$zip))
stopifnot(!anyNA(actual_hash), all(actual_hash == manifest$zip_md5))
record('Stored summary archives agree with recorded MD5', 0, 0)
raw <- fread('audit/review_20261015/results/raw_checks.csv')
stopifnot(nrow(raw) == 13L, all(raw$roster_disagreements == 0), all(raw$max_weight_error < 1e-8))
record('All-wave roster child count disagreements', max(raw$roster_disagreements), 0)
# The independent revision script writes these files only after its numerical assertions.
# The subsequent process review also checks a fresh isolated production rebuild.
for (f in c('audit/review_20261015/revision_7121ec0/results/independent_bands.csv',
            'audit/review_20261015/revision_7b3f727/results/independent_definitions.csv',
            'audit/review_20261015/revision_7b3f727/results/independent_period_change.csv')) {
  z <- fread(f)
  record(paste(basename(f), 'point estimates'), max(abs(z$est_error)), 1e-8)
  record(paste(basename(f), 'standard errors'), max(abs(z$se_error)), 1e-7)
}
pool <- fread('audit/review_20261015/revision_7b3f727/results/pooled_rank_independent_wave_check.csv')
stopifnot(nrow(pool) == 2L, all(pool$lower > 0))
# Compare every production data object, numeric CSV and figure to the fresh rebuild.
repro <- 'audit/review_20261015/reproduction'
clean <- function(x) {
  attr(x, 'built_at') <- NULL
  if (is.data.frame(x)) return(as.data.frame(x))
  if (is.list(x)) {
    x$built_at <- NULL
    if (!is.null(x$sample_note)) x$sample_note$built_at <- NULL
    x <- lapply(x, clean)
  }
  x
}
rebuild <- list()
for (f in c('data/scf_linked.rds', paste0('output/', c('kids_main','kids_income','kids_blended',
  'kids_wealth_work','kids_rep5','kids_top1_defs','kids_period_change','kids_bin_ci','kids_count_checks'), '.rds'))) {
  eq <- isTRUE(all.equal(clean(readRDS(f)), clean(readRDS(file.path(repro, f))), check.attributes = FALSE))
  record(paste('Fresh rebuild:', f), as.numeric(!eq), 0)
  rebuild[[f]] <- data.table(file = f, matches = eq, comparison = 'Values, excluding build timestamps')
}
a_csv <- fread('graphics/post_numbers.csv'); b_csv <- fread(file.path(repro, 'graphics/post_numbers.csv'))
eq <- isTRUE(all.equal(a_csv[, !'built_at'], b_csv[, !'built_at'], check.attributes = FALSE))
record('Fresh rebuild: graphics/post_numbers.csv', as.numeric(!eq), 0)
rebuild[['numbers']] <- data.table(file = 'graphics/post_numbers.csv', matches = eq, comparison = 'Values, excluding build timestamps')
for (f in list.files('graphics', pattern = '\\.png$', full.names = TRUE)) {
  eq <- identical(unname(tools::md5sum(f)), unname(tools::md5sum(file.path(repro, f))))
  record(paste('Fresh rebuild:', f), as.numeric(!eq), 0)
  rebuild[[f]] <- data.table(file = f, matches = eq, comparison = 'Byte-for-byte PNG MD5')
}
rebuild <- rbindlist(rebuild)
fwrite(rebuild, 'audit/results/process_factcheck_20261016_rebuild.csv')
process <- jsonlite::read_json('audit/results/process_factcheck_20261016.json', simplifyVector = TRUE)
stopifnot(process$summary_rows == nrow(readRDS('data/scf_linked.rds')),
          all(process$waves$sample_rank_weight_disagreements == 0),
          all(process$replicate_weights$missing_weight_ids == 0))
main <- readRDS('output/kids_main.rds')
rec <- main[year >= 2013]
repository_head <- trimws(system2('git', c('rev-parse', 'HEAD'), stdout = TRUE))
reviewed_commit <- trimws(system2('git', c('log', '-1', '--format=%H', '--', 'blog_post_text.md'), stdout = TRUE))
committed_blob <- trimws(system2('git', c('rev-parse', 'HEAD:blog_post_text.md'), stdout = TRUE))
working_blob <- trimws(system2('git', c('hash-object', 'blog_post_text.md'), stdout = TRUE))
stopifnot(identical(committed_blob, working_blob))
summary <- list(
  reviewed_date = '2026-10-16', model = 'GPT-6.1 Sol (high reasoning)',
  blog_md5 = unname(tools::md5sum('blog_post_text.md')),
  reviewed_commit = reviewed_commit, repository_head = repository_head, reviewed_blog_blob = committed_blob, working_copy_matches_commit = TRUE,
  checks = rbindlist(results), rebuild = rebuild, stock = as.list(shares),
  headline = r[grepl('^all_', stat)], definitions = td, counting = cc, period = pc,
  numbers = n, raw_integrity = raw,
  overall = main[, .(share3 = 100 * weighted.mean(kids >= 3, w)), by = period],
  sample = list(recent_families = uniqueN(rec$case), recent_rows = nrow(rec),
                top_income_any_implicate = uniqueN(rec[inc_rank > .99, case])),
  supplementary_measurement = fread('audit/results/measurement_response_20261016.csv'))
dir.create('audit/results', showWarnings = FALSE)
fwrite(rbindlist(results), 'audit/results/factcheck_20261016_checks.csv')
write_json(summary, 'audit/results/factcheck_20261016.json', dataframe = 'rows', auto_unbox = TRUE, digits = 15, pretty = TRUE)
print(rbindlist(results))
message('Fact-check assertions passed; wrote dated evidence JSON and check table.')
