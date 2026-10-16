# 26_top1_definitions.R
# Does the top-1% result depend on how "top 1%" is defined? Heads 45-64, pooled 2013-2025.
#
# Outcome is fixed: share of families with 3+ children (kids = KIDS + max(X5910, 0), from R/07).
# Each row changes only the ranking used to pick the top group and its comparison group
# (the next group down, ending at the 90th percentile), then reruns the R/25 inference:
#  - Equal weight per wave; pooled share = ratio of summed moments.
#  - Ranks re-estimated inside every replicate (999 Fed replicate weights, implicate 1).
#  - Waves independent: per-wave replicate variances summed through the gradient of the gap.
#    For the pooled-across-years ranking, ranks depend on all waves at once, so the variance
#    is taken over replicate draws of the summed moments instead (same estimand).
#  - Imputation variance 1.2 B across the five implicates.
# Point estimates and SEs for the baseline row should match R/25 (all_gap3).
# Writes output/kids_top1_defs.rds and definitions.html.
source("R/scf_functions.R")
suppressPackageStartupMessages({ library(haven) })

years <- seq(2013, 2025, 3)
main <- readRDS("output/kids_main.rds")[year >= 2013, .(year, Y1, kids)]
sx <- readRDS("data/scf_linked.rds")[year >= 2013, .(year, Y1, implicate, WGT, AGE, INCOME, NORMINC, NETWORTH)]
sx[, inc_measure := fifelse(is.na(NORMINC), INCOME, NORMINC)]
sx <- merge(sx, main, by = c("year", "Y1"), all.x = TRUE)
# kids is defined exactly for heads 45-64 (the R/07 sample); everyone else is ranking population only
stopifnot(sx[!is.na(kids), all(AGE >= 45 & AGE <= 64)], sx[AGE >= 45 & AGE <= 64, !anyNA(kids)])
sx[, in_s := !is.na(kids)]
sx[, age5 := floor(AGE / 5) * 5]
sx[, age10 := fifelse(AGE < 55, 45, 55)]
setorder(sx, year, Y1)   # same tie order as R/07 and R/25

# Weighted mid-rank, as in R/07 / R/25.
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); r[o] <- (cumsum(w[o]) - w[o] / 2) / sum(w); r }

defs <- data.table(
  id    = c("base", "age10", "age4564", "allpop", "pooled", "actual", "top2", "top05"),
  label = c("Current definition", "Wider age bands", "One ranking for all 45-64",
            "Whole population", "Pooled across 2013-2025", "Actual income",
            "Top 2%", "Top 0.5%"),
  expl  = c("Ranked within survey year and 5-year age band of the head (45-49, 50-54, 55-59, 60-64). Top 1% vs. 90th-99th.",
            "Ranked within survey year and 10-year age band (45-54, 55-64).",
            "Ranked within survey year against all families with a head aged 45-64, ignoring age bands.",
            "Ranked within survey year against all US families of every age, then the 45-64 families are kept. The top 1% is the national top 1%.",
            "One ranking of all heads aged 45-64 across the five surveys combined (2025 dollars, each survey weighted equally). A common pooled ranking in 2025 dollars.",
            "Current age-band ranking, but on last year's actual income instead of usual income. Income only.",
            "Current age-band ranking, top 2% vs. 90th-98th.",
            "Current age-band ranking, top 0.5% vs. 90th-99.5th."),
  pop   = c("s", "s", "s", "all", "s", "s", "s", "s"),
  cell  = c("age5", "age10", "none", "none", "none", "age5", "age5", "age5"),
  pooled = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
  incvar = c("inc_measure", "inc_measure", "inc_measure", "inc_measure", "inc_measure", "INCOME", "inc_measure", "inc_measure"),
  topcut = c(.99, .99, .99, .99, .99, .99, .98, .995))

# Ranks for one draw. L: list over waves of data.tables (one implicate); W: matching weights.
ranks_for <- function(L, W, d, v) {
  if (d$pooled) {
    # one ranking over in-sample families of all waves, each wave's weight summing to 1
    xs <- unlist(lapply(L, function(s) s[[v]][s$in_s]))
    ws <- unlist(Map(function(s, w) { w <- w[s$in_s]; w / sum(w) }, L, W))
    r_all <- prank(xs, ws)
    n <- vapply(L, function(s) sum(s$in_s), 1L); off <- c(0, cumsum(n))
    return(lapply(seq_along(L), function(k) { r <- rep(NA_real_, nrow(L[[k]])); r[L[[k]]$in_s] <- r_all[(off[k] + 1):off[k + 1]]; r }))
  }
  Map(function(s, w) {
    r <- rep(NA_real_, nrow(s))
    pop <- if (d$pop == "all") rep(TRUE, nrow(s)) else s$in_s
    cell <- if (d$cell == "none") rep(0, nrow(s)) else s[[d$cell]]
    for (cv in unique(cell[pop])) { j <- which(pop & cell == cv); r[j] <- prank(s[[v]][j], w[j]) }
    r
  }, L, W)
}
# Moments per wave: (den_top, num_top, den_aff, num_aff), weights normalized over in-sample families.
moments <- function(s, w, r, topcut) {
  k <- s$in_s; w <- w[k] / sum(w[k]); r <- r[k]; y <- s$kids[k] >= 3
  top <- r > topcut; aff <- r > .9 & r <= topcut
  c(sum(w[top]), sum(w[top] * y[top]), sum(w[aff]), sum(w[aff] * y[aff]))
}
gapfun <- function(m) 100 * (m[2] / m[1] - m[4] / m[3])
gradfun <- function(m) 100 * c(-m[2] / m[1]^2, 1 / m[1], m[4] / m[3]^2, -1 / m[3])

# Data by wave and implicate; replicate weights for implicate 1.
by_imp <- lapply(1:5, function(i) lapply(years, function(y) sx[year == y & implicate == i]))
L1 <- by_imp[[1]]
RW <- lapply(seq_along(years), function(k) {
  rw <- as.data.table(zap_labels(read_dta(sprintf("data_raw/rw/p%02d_rw1.dta", years[k] %% 100))))
  setnames(rw, tolower(names(rw)))
  ix <- match(L1[[k]]$Y1, rw$y1)
  if (anyNA(ix)) stop("replicate weights missing for some implicate-1 cases in ", years[k])
  m <- sapply(1:999, function(b) rw[[paste0("wt1b", b)]][ix] * rw[[paste0("mm", b)]][ix])
  m[is.na(m)] <- 0
  m
})

run_def <- function(d, rk) {
  v <- if (rk == "income") d$incvar else "NETWORTH"
  # point estimates, implicate by implicate
  imp <- lapply(1:5, function(i) {
    L <- by_imp[[i]]; W <- lapply(L, `[[`, "WGT"); R <- ranks_for(L, W, d, v)
    per <- Map(function(s, w, r) moments(s, w, r, d$topcut), L, W, R)
    list(tot = Reduce(`+`, per), top_n = sum(vapply(Map(function(s, r) sum(s$in_s & r > d$topcut, na.rm = TRUE), L, R), identity, 1)))
  })
  Q <- vapply(imp, function(z) gapfun(z$tot), 1)
  shares <- sapply(imp, function(z) 100 * c(z$tot[2] / z$tot[1], z$tot[4] / z$tot[3]))
  g <- gradfun(imp[[1]]$tot)
  # replicate draws
  draws <- array(0, c(4, length(years), 999))
  for (b in 1:999) {
    W <- lapply(RW, function(m) m[, b]); R <- ranks_for(L1, W, d, v)
    for (k in seq_along(years)) draws[, k, b] <- moments(L1[[k]], W[[k]], R[[k]], d$topcut)
  }
  V <- if (d$pooled) var(as.vector(g %*% apply(draws, c(1, 3), sum)))
       else sum(vapply(seq_along(years), function(k) var(as.vector(g %*% draws[, k, ])), 1))
  se <- sqrt(V + 1.2 * var(Q))
  est <- mean(Q)
  data.table(id = d$id, rank = rk, top_share = mean(shares[1, ]), aff_share = mean(shares[2, ]),
             gap = est, se = se, lower = est - 1.96 * se, upper = est + 1.96 * se,
             p = 2 * pnorm(-abs(est / se)), top_cases = imp[[1]]$top_n)
}

res <- list()
for (i in seq_len(nrow(defs))) for (rk in c("income", "wealth")) {
  d <- defs[i]
  if (d$id == "actual" && rk == "wealth") next
  res[[paste(d$id, rk)]] <- run_def(d, rk)
  message(sprintf("%-8s %-6s gap %+6.2f  SE %.2f", d$id, rk, res[[paste(d$id, rk)]]$gap, res[[paste(d$id, rk)]]$se))
}
out <- defs[rbindlist(res), on = "id"]
attr(out, "built_at") <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")

# Sanity check: baseline must reproduce R/25.
r25 <- readRDS("output/kids_rep5.rds")[stat == "all_gap3"]
chk <- merge(out[id == "base", .(rank = fifelse(rank == "income", "inc_rank", "nw_rank"), gap, se)], r25[, .(rank, est, se25 = se)], by = "rank")
print(chk)
stopifnot(chk[, all(abs(gap - est) < 1e-6 & abs(se - se25) < 0.05)])
saveRDS(out, "output/kids_top1_defs.rds")
options(width = 200)
print(out[, .(id, rank, top_share, aff_share, gap, se, lower, upper, p, top_cases)], digits = 3)

# ---- definitions.html -------------------------------------------------------------------
esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x))
cellhtml <- function(r, lab) {
  if (is.null(r) || !nrow(r)) return(sprintf('<td class="na" data-label="%s">Not applicable</td>', lab))
  sig <- r$lower > 0
  sprintf('<td data-label="%s"><div class="gap">%+.1f pts</div><div class="ci">95%% CI %.1f to %.1f</div><div class="lv">%.0f%% vs. %.0f%%</div><span class="badge %s">%s</span></td>',
          lab, r$gap, r$lower, r$upper, r$top_share, r$aff_share, if (sig) "yes" else "no", if (sig) "Significant" else "Not significant")
}
rows <- vapply(seq_len(nrow(defs)), function(i) {
  d <- defs[i]
  sprintf('<tr%s><th scope="row"><div class="def">%s</div><div class="expl">%s</div></th>%s%s</tr>',
          if (d$id == "base") ' class="base"' else "", esc(d$label), esc(d$expl),
          cellhtml(out[id == d$id & rank == "income"], "Ranked by income"), cellhtml(out[id == d$id & rank == "wealth"], "Ranked by wealth"))
}, "")
nsig <- out[, sum(lower > 0)]; ntot <- nrow(out)
html <- paste0('<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Top 1% definitions</title>
<style>
:root{--bg:#fff;--fg:#1d1d1b;--muted:#5f5e5a;--line:#e4e2da;--base:#f5f3ec;--yes-bg:#eaf3de;--yes-fg:#27500a;--no-bg:#fcebeb;--no-fg:#791f1f}
@media (prefers-color-scheme:dark){:root{--bg:#1b1b19;--fg:#ecebe6;--muted:#a9a79f;--line:#3a3935;--base:#26251f;--yes-bg:#27500a;--yes-fg:#eaf3de;--no-bg:#791f1f;--no-fg:#fcebeb}}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 -apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif}
main{max-width:1000px;margin:0 auto;padding:32px 16px 48px}
h1{font-size:22px;font-weight:600;margin:0 0 6px}
p.lede{color:var(--muted);margin:0 0 20px;max-width:760px}
.wrap{overflow-x:auto}
table{border-collapse:collapse;width:100%;min-width:640px}
thead th{text-align:left;font-size:13px;color:var(--muted);font-weight:600;border-bottom:1px solid var(--line);padding:8px 10px}
td,tbody th{vertical-align:top;border-bottom:1px solid var(--line);padding:12px 10px;text-align:left;font-weight:400}
tbody th{width:46%}
tr.base{background:var(--base)}
.def{font-weight:600}
.expl{color:var(--muted);font-size:13px;margin-top:2px}
.gap{font-weight:600;font-variant-numeric:tabular-nums}
.ci,.lv{color:var(--muted);font-size:13px;font-variant-numeric:tabular-nums}
.na{color:var(--muted);font-size:13px}
.badge{display:inline-block;margin-top:6px;padding:1px 8px;border-radius:10px;font-size:12px;font-weight:600}
.badge.yes{background:var(--yes-bg);color:var(--yes-fg)}.badge.no{background:var(--no-bg);color:var(--no-fg)}
.notes{color:var(--muted);font-size:13px;margin-top:20px;max-width:820px}
.notes li{margin-bottom:4px}
@media (max-width:640px){table{min-width:0}thead{display:none}tr{display:block;border-bottom:1px solid var(--line);padding:4px 0}tbody th,td{display:block;width:auto;border:0;padding:8px 10px}td::before{content:attr(data-label);display:block;font-size:12px;color:var(--muted);font-weight:600}}
</style></head><body><main>
<h1>Does the result depend on how we define the 1%?</h1>
<p class="lede">Gap in the share of families with three or more children: top group minus the rest of the top 10% under the same ranking. ',
nsig, ' of ', ntot, ' tests are significant at the 5% level. Families with a head aged 45-64, five surveys pooled (2013-2025).</p>
<div class="wrap"><table>
<thead><tr><th>Definition of the top group</th><th>Ranked by income</th><th>Ranked by wealth</th></tr></thead>
<tbody>', paste(rows, collapse = "\n"), '</tbody></table></div>
<ul class="notes">
<li>Each cell shows the gap in percentage points, its 95% confidence interval, and the two groups\' shares with 3+ children. "Significant" means the interval excludes zero (two-sided, 5% level).</li>
<li>Income is usual income unless stated. Wealth is net worth. Children = children of the head or partner at home plus those living elsewhere (an imperfect family count, not births).</li>
<li>Standard errors use the Fed\'s 999 replicate weights with the ranks re-estimated in every replicate, plus imputation variance across the five implicates. Intervals are pointwise and not adjusted for testing several definitions.</li>
<li>Source: Federal Reserve Survey of Consumer Finances; <code>R/26_top1_definitions.R</code>. Built ', attr(out, "built_at"), '.</li>
</ul></main></body></html>')
writeLines(html, "definitions.html")
message("Wrote output/kids_top1_defs.rds and definitions.html")
