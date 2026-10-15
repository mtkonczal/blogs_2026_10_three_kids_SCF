# 24_kids_blended.R
# How much do blended families affect the kids-by-income/wealth results? Heads 45-64, 2013-2025.
#
# The public SCF cannot separate stepchildren: the household roster code 4 is "child (in-law) of R or
# spouse/partner", and X5910 (children living elsewhere) explicitly includes children from previous
# marriages of either partner. What we can flag is a couple where either partner was married before
# (X7377 / X7392 == 2). Those are the only families where a partner's children from an earlier
# relationship get added to the count. Never-married cohabiting partners are coded 0 (inap.) and are
# treated as not previously married.
# Test: drop "remarried couples" and recompute, keeping each family's percentile rank from the full
# sample so the bins mean the same thing.
source("R/scf_functions.R")
source("R/rep_se.R")
main <- readRDS("output/kids_main.rds")
rec <- main[period == "2013-2025"]
rec[, remarried := MARRIED == 1 & (x7377 == 2 | x7392 == 2)]
main[, remarried := MARRIED == 1 & (x7377 == 2 | x7392 == 2)]

summ <- function(s) list(remarried = 100 * wmean(s$remarried, s$w),
                         share3p_all = 100 * wmean(s$kids >= 3, s$w),
                         share3p_excl = 100 * wmean(s$kids[!s$remarried] >= 3, s$w[!s$remarried]),
                         share5p_all = 100 * wmean(s$kids >= 5, s$w),
                         share5p_excl = 100 * wmean(s$kids[!s$remarried] >= 5, s$w[!s$remarried]),
                         mean_all = wmean(s$kids, s$w), mean_excl = wmean(s$kids[!s$remarried], s$w[!s$remarried]),
                         n_fam = uniqueN(s$case))
out <- list()
out$by_income <- rec[, summ(.SD), by = inc_bin][order(inc_bin)]
out$by_wealth <- rec[, summ(.SD), by = nw_bin][order(nw_bin)]
# How much bigger are remarried couples' counts? Compare with first-marriage couples, same bins
cmp <- function(s) list(first_marr = wmean(s$kids[s$first_marriage_couple %in% TRUE], s$w[s$first_marriage_couple %in% TRUE]),
                        remarried = wmean(s$kids[s$remarried], s$w[s$remarried]))
out$couples_overall <- rec[MARRIED == 1, cmp(.SD)]
out$couples_by_wealth <- rec[MARRIED == 1, cmp(.SD), by = nw_bin][order(nw_bin)]

# Replicate-weight check of the headline gaps (top 1% vs 90th-99th), excluding remarried couples
gap <- function(rk) function(s, w) {
  k <- !s$remarried
  g <- function(lo, hi) { j <- k & s[[rk]] > lo & s[[rk]] <= hi; 100 * sum(w[j] * (s$kids[j] >= 3)) / sum(w[j]) }
  m <- function(lo, hi) { j <- k & s[[rk]] > lo & s[[rk]] <= hi; sum(w[j] * s$kids[j]) / sum(w[j]) }
  c(gap3 = g(.99, 1) - g(.9, .99), gapk = m(.99, 1) - m(.9, .99), top1_3p = g(.99, 1), p90_99_3p = g(.9, .99))
}
rw <- rbindlist(lapply(c("inc_rank", "nw_rank"), function(rk) {
  r <- lapply(c(2019, 2022, 2025), function(yr) rep_stat(main[year == yr], gap(rk), yr))
  data.table(rank = rk, stat = names(r[[1]]$est), est = rowMeans(sapply(r, `[[`, "est")),
             se = sqrt(rowSums(sapply(r, function(x) x$se^2))) / 3)
}))
out$rw_excl_remarried <- rw
out$built_at <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
saveRDS(out, "output/kids_blended.rds")
options(width = 200)
for (n in names(out)) if (is.data.frame(out[[n]])) { cat("\n==", n, "\n"); print(out[[n]], digits = 3) }
