# 23_kids_wealth_work.R
# Exploration for the kids post: do wealthy families who don't need to work much show the
# same big-family pattern? Heads 45-64, pooled 2013-2025 (reads output/kids_main.rds from 07).
#
# Ranks (all within survey year x implicate x 5-year age band, like 07):
#   nw_rank = net worth (from 07); wage_rank = wages and salaries (X5702).
# Groups use top-5% cutoffs because a top-1% x top-1% cross-tab is too thin.
# Work hours: X4110 (head) + X4710 (spouse/partner), usual weekly hours on the main job; 0 = not working.
# All of these are measured at 45-64, AFTER the children were born, so work and wealth are partly
# outcomes of having kids (e.g., a parent leaving work). Descriptive only.
# SEs: within-implicate cluster-robust (family) + Rubin's rules; ignore SCF design, approximate.
source("R/scf_functions.R")
suppressPackageStartupMessages({ library(haven); library(fixest) })
main <- readRDS("output/kids_main.rds")[period == "2013-2025"]
files <- c(`2013` = "p13i6", `2016` = "p16i6", `2019` = "p19i6", `2022` = "p22i6", `2025` = "p25i6")
cols <- c("y1", "x5702", "x5729", "x4110", "x4710")
inc <- rbindlist(lapply(names(files), function(yr) {
  f <- sprintf("data_raw/full/%s.dta", files[[yr]]); nm <- names(read_dta(f, n_max = 1))
  d <- as.data.table(zap_labels(read_dta(f, col_select = dplyr::all_of(nm[tolower(nm) %in% cols]))))
  setnames(d, tolower(names(d))); d[, year := as.numeric(yr)][]
}))
d <- merge(main, inc, by.x = c("year", "Y1"), by.y = c("year", "y1"))
stopifnot(nrow(d) == nrow(main))
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); cw <- cumsum(w[o]); r[o] <- (cw - w[o] / 2) / sum(w); r }
d[, wage := pmax(x5702, 0)]
d[, wage_rank := prank(wage, WGT), by = .(year, implicate, age5)]
d[, y3 := 100 * (kids >= 3)]
# Remarried couple: either partner married before, so stepchildren can be in the count (see R/24)
d[, remarried := MARRIED == 1 & (x7377 == 2 | x7392 == 2)]
d[, hours := pmax(x4110, 0) + fifelse(MARRIED == 1, pmax(x4710, 0), 0)]
d[, wage_share := fifelse(x5729 > 0, pmin(wage / x5729, 1), NA_real_)]

rubin <- function(fits) { b <- sapply(fits, coef); v <- sapply(fits, function(f) diag(vcov(f))); M <- ncol(b)
  data.table(term = rownames(b), est = rowMeans(b), se = sqrt(rowMeans(v) + (1 + 1 / M) * apply(b, 1, var))) }
grp_tab <- function(s, g) {
  s <- s[!is.na(get(g))]
  r3 <- rubin(lapply(1:5, function(i) feols(as.formula(paste("y3 ~ 0 +", g)), data = s[implicate == i], weights = ~w, cluster = ~case)))
  rk <- rubin(lapply(1:5, function(i) feols(as.formula(paste("kids ~ 0 +", g)), data = s[implicate == i], weights = ~w, cluster = ~case)))
  n <- s[, .(n_fam = uniqueN(case), med_nw_m = wq(NETWORTH, w) / 1e6, med_wage_k = wq(wage, w) / 1e3,
             med_hours = wq(hours, w), married = 100 * wmean(MARRIED == 1, w)), by = g]
  out <- data.table(group = sub(paste0("^", g), "", r3$term), share3p = r3$est, se3 = r3$se, kids = rk$est, se_k = rk$se)
  merge(out, n[, group := as.character(get(g))][, (g) := NULL], by = "group")
}
out <- list()

# 1. Wealth instead of income: the curve by net-worth percentile
out$by_wealth <- grp_tab(d, "nw_bin")[order(match(group, levels(d$nw_bin)))]

# 2. Rich vs. high earners, separated
d[, rich_vs_earner := fcase(
  nw_rank > .95 & wage_rank <= .90, "A. Top 5% wealth, wages below top 10%",
  nw_rank > .95 & wage_rank > .95,  "B. Top 5% wealth and top 5% wages",
  wage_rank > .95 & nw_rank <= .90, "C. Top 5% wages, wealth below top 10%",
  inc_rank > .4 & inc_rank <= .6,   "D. Middle (40th-60th income)",
  default = NA_character_)]
out$rich_vs_earner <- grp_tab(d, "rich_vs_earner")[order(group)]

# 3. Within the top 5% of wealth: how much does the family work?
top5 <- d[nw_rank > .95]
top5[, work := factor(fcase(hours == 0, "No one works", hours < 40, "Under 40 hrs/week combined",
                            hours < 80, "40-79 hrs/week combined", default = "80+ hrs/week combined"),
                      c("No one works", "Under 40 hrs/week combined", "40-79 hrs/week combined", "80+ hrs/week combined"))]
out$top5w_by_hours <- grp_tab(top5, "work")[order(match(group, levels(top5$work)))]
# Same split for the top 1% of wealth, coarser
top1 <- d[nw_rank > .99]
top1[, work2 := fifelse(hours < 40, "Under 40 hrs combined", "40+ hrs combined")]
out$top1w_by_hours <- grp_tab(top1, "work2")
# Wage share of income within the top 1% of wealth
top1[, live_off := fcase(wage_share < .25, "Wages < 25% of income", wage_share >= .25, "Wages >= 25% of income", default = NA_character_)]
out$top1w_by_wageshare <- grp_tab(top1, "live_off")
# Same two splits without remarried couples
out$top1w_by_hours_excl_remarried <- grp_tab(top1[remarried == FALSE], "work2")
out$top1w_by_wageshare_excl_remarried <- grp_tab(top1[remarried == FALSE], "live_off")
# Difference (under 40 hrs minus 40+ hrs) with Rubin SE, both samples
hdiff <- function(s) { s <- copy(s)[, low := as.numeric(hours < 40)]
  r <- rubin(lapply(1:5, function(i) feols(y3 ~ low, data = s[implicate == i], weights = ~w, cluster = ~case))); r[term == "low"] }
out$top1w_hours_gap <- rbind(hdiff(top1)[, sample := "All families"], hdiff(top1[remarried == FALSE])[, sample := "Excluding remarried couples"])

out$built_at <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
saveRDS(out, "output/kids_wealth_work.rds")
options(width = 200)
for (n in names(out)) if (is.data.frame(out[[n]])) { cat("\n==", n, "\n"); print(out[[n]], digits = 3) }
