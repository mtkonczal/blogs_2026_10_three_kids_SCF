# 07_kids_income.R
# Income and number of children in the SCF, 1989-2025.
#
# Fertility measure ("living children"): children of the respondent or spouse/partner
# living in the household (KIDS, summary extract; relationship codes child/stepchild/
# foster) PLUS sons and daughters of the respondent or spouse who live elsewhere
# (X5910, full public data, asked in every wave since 1989; top-coded at 10).
# For families whose reference person is 45-64 this approximates completed fertility.
# It includes stepchildren and adopted children and excludes children who have died.
#
# Income rank: percentile of usual income (NORMINC; actual INCOME in 1989/1992, when
# usual income was not asked) within survey year x implicate x 5-year age band of the
# reference person, so the rank compares families at the same life-cycle stage.
#
# Inference: point estimates pool the 5 implicates. Regression SEs combine within-
# implicate cluster-robust variance (clustered on family) with between-implicate
# variance by Rubin's rules. Replicate weights are not used, so SEs ignore the SCF's
# stratified design and are approximate.
#
# Output: output/kids_main.rds (analysis sample, read by R/22-24) and output/kids_income.rds
# (exploratory tables printed below; not used by the post).

source("R/scf_functions.R")
suppressPackageStartupMessages({ library(haven); library(fixest) })

files <- c(`1989` = "p89i6", `1992` = "p92i4", `1995` = "p95i6", `1998` = "p98i6", `2001` = "p01i6",
           `2004` = "p04i6", `2007` = "p07i6", `2010` = "p10i6", `2013` = "p13i6", `2016` = "p16i6",
           `2019` = "p19i6", `2022` = "p22i6", `2025` = "p25i6")
read_kids <- function(y) {
  f <- sprintf("data_raw/full/%s.dta", files[[y]])
  nm <- names(read_dta(f, n_max = 1))
  want <- c("y1", "x1", "x5910", "x7377", "x7392", "x4100", "x4700",
            # sex of reference person / spouse; spouse education (new coding 2016+, old coding <=2013)
            "x8021", "x103", "x6111", "x6112", "x6101", "x6102", "x6104", "x6105")
  d <- as.data.table(zap_labels(read_dta(f, col_select = dplyr::all_of(nm[tolower(nm) %in% want]))))
  setnames(d, tolower(names(d)))
  if ("x1" %in% names(d)) setnames(d, "x1", "y1")
  for (v in c("x7377", "x7392", "x6111", "x6112", "x6101", "x6102", "x6104", "x6105"))
    if (!v %in% names(d)) d[, (v) := NA_real_]
  d[, year := as.numeric(y)][]
}
fk <- rbindlist(lapply(names(files), read_kids), use.names = TRUE)
sx <- readRDS("data/scf_linked.rds")
sx <- sx[, .(year, Y1, YY1, implicate, WGT, AGE, KIDS, MARRIED, HHSEX, EDCL, RACECL4, INCOME, NORMINC, NETWORTH)]
d <- merge(sx, fk, by.x = c("year", "Y1"), by.y = c("year", "y1"))
stopifnot(nrow(d) == nrow(sx))
d[, kids_away := pmax(x5910, 0)]
d[, kids := KIDS + kids_away]
d[, first_marriage_couple := MARRIED == 1 & x7377 == 1 & x7392 == 1]  # NA before 1995
# Work status recode: 11-17 = working (worker only, or worker + another status)
d[, `:=`(r_works = x4100 %in% 11:17, sp_works = x4700 %in% 11:17)]
d[, earners := fifelse(MARRIED == 1, fifelse(r_works & sp_works, "Two earners", "One or no earner"), NA_character_)]
d[, period := factor(fcase(year <= 1998, "1989-1998", year <= 2010, "2001-2010", default = "2013-2025"),
                     c("1989-1998", "2001-2010", "2013-2025"))]
d[, inc_measure := fifelse(is.na(NORMINC), INCOME, NORMINC)]
d[, age5 := floor(AGE / 5) * 5]
d[, case := paste(year, YY1)]

# Weighted percentile rank (midpoint of cumulative weight) within groups
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); cw <- cumsum(w[o]); r[o] <- (cw - w[o] / 2) / sum(w); r }
bin_lv <- c("P0-10", "P10-20", "P20-30", "P30-40", "P40-50", "P50-60", "P60-70", "P70-80", "P80-90",
            "P90-95", "P95-99", "Top 1%")
binit <- function(r) factor(as.character(cut(r, c(0, seq(.1, .9, .1), .95, .99, 1), labels = bin_lv, include.lowest = TRUE)), bin_lv)

make_sample <- function(age_lo, age_hi, outcome = "kids") {
  s <- d[AGE >= age_lo & AGE <= age_hi]
  s[, inc_rank := prank(inc_measure, WGT), by = .(year, implicate, age5)]
  s[, nw_rank := prank(NETWORTH, WGT), by = .(year, implicate, age5)]
  s[, `:=`(inc_bin = binit(inc_rank), nw_bin = binit(nw_rank))]
  s[, y := get(outcome)]
  # each wave gets equal total weight within a period
  s[, w := WGT / sum(WGT), by = .(year, implicate)]
  s[]
}
main <- make_sample(45, 64)
message("Main sample (head 45-64): ", main[, uniqueN(case)], " families; ",
        main[, .N], " family-implicate rows")

# ---- Descriptive curves -----------------------------------------------------------
curve <- function(s, bin = "inc_bin", by = "period") {
  s[, .(mean_kids = wmean(y, w), share0 = 100 * wmean(y == 0, w), share1 = 100 * wmean(y == 1, w),
        share2 = 100 * wmean(y == 2, w), share3p = 100 * wmean(y >= 3, w), share4p = 100 * wmean(y >= 4, w),
        ppr1 = 100 * wmean(y >= 1, w), ppr2 = 100 * sum(w[y >= 2]) / sum(w[y >= 1]),
        ppr3 = 100 * sum(w[y >= 3]) / sum(w[y >= 2]), ppr4 = 100 * sum(w[y >= 4]) / sum(w[y >= 3]),
        n_fam = uniqueN(case), med_inc = wq(inc_measure, w) / 1000),
    by = c(by, bin)][order(get(by), get(bin))]
}
out <- list()
out$inc_curve <- curve(main)
out$nw_curve <- curve(main, "nw_bin")
setnames(out$nw_curve, "nw_bin", "inc_bin")
out$married_curve <- curve(main[MARRIED == 1])
out$firstmarr_curve <- curve(main[first_marriage_couple == TRUE])
out$single_curve <- curve(main[MARRIED == 2])
out$by_educ <- main[, .(mean_kids = wmean(y, w), share3p = 100 * wmean(y >= 3, w), share0 = 100 * wmean(y == 0, w),
                        n_fam = uniqueN(case)), by = .(period, EDCL)][order(period, EDCL)]
out$overall <- main[, .(mean_kids = wmean(y, w), share0 = 100 * wmean(y == 0, w), share3p = 100 * wmean(y >= 3, w),
                        ppr3 = 100 * sum(w[y >= 3]) / sum(w[y >= 2]), n_fam = uniqueN(case)), by = period]
# Wave-by-wave: top 10% vs middle (P40-60) vs bottom 20% share with 3+
main[, grp3 := fcase(inc_rank <= .2, "Bottom 20%", inc_rank > .4 & inc_rank <= .6, "Middle (P40-60)",
                     inc_rank > .9, "Top 10%", default = NA_character_)]
out$by_wave <- main[!is.na(grp3), .(share3p = 100 * wmean(y >= 3, w), mean_kids = wmean(y, w)), by = .(year, grp3)][order(year, grp3)]

# Composition of the top: married share by bin (recent period)
out$married_by_bin <- main[period == "2013-2025", .(married = 100 * wmean(MARRIED == 1, w),
  first_marr = 100 * wmean(first_marriage_couple == TRUE, w)), by = inc_bin][order(inc_bin)]

# Dual-earner couples: a descriptive check on the "career compatibility" mechanism.
# Work status at 45-64 is partly a consequence of past fertility, so this is not causal.
mc <- main[period == "2013-2025" & MARRIED == 1]
mc[, band := factor(fcase(inc_rank <= .5, "Bottom half", inc_rank <= .8, "P50-80", inc_rank <= .99, "P80-99", default = "Top 1%"),
                    c("Bottom half", "P50-80", "P80-99", "Top 1%"))]
out$dual_rate <- mc[, .(two_earner = 100 * wmean(earners == "Two earners", w), n_fam = uniqueN(case)), by = band][order(band)]
out$dual_kids <- mc[, .(share3p = 100 * wmean(kids >= 3, w), mean_kids = wmean(kids, w), n_fam = uniqueN(case)),
                    by = .(band, earners)][order(band, earners)]

# Younger families, children at home only (age 35-44): fertility in progress
young <- make_sample(35, 44, "KIDS")
out$young_curve <- curve(young)

# ---- Regressions with Rubin's rules ----------------------------------------------
rubin <- function(fits) {
  b <- sapply(fits, coef); v <- sapply(fits, function(f) diag(vcov(f)))
  m <- nrow(b); M <- ncol(b)
  qbar <- rowMeans(b); W <- rowMeans(v); B <- apply(b, 1, var)
  T <- W + (1 + 1 / M) * B
  data.table(term = rownames(b), est = qbar, se = sqrt(T))
}
run_reg <- function(s, fml, ref = "P40-50") {
  s <- copy(s)
  s[, inc_bin := relevel(inc_bin, ref = ref)]
  fits <- lapply(1:5, function(i) feols(fml, data = s[implicate == i], weights = ~w, cluster = ~case))
  r <- rubin(fits)
  r[grepl("^inc_bin", term)][, term := sub("^inc_bin", "", term)][]
}
rec <- main[period == "2013-2025"]
rec[, y3 := 100 * (kids >= 3)]
specs <- list(
  "Age and year only" = y3 ~ inc_bin + i(AGE) + i(year),
  "+ race, education, marital status" = y3 ~ inc_bin + i(AGE) + i(year) + i(RACECL4) + i(EDCL) + i(MARRIED),
  "Married couples, + race, education" = y3 ~ inc_bin + i(AGE) + i(year) + i(RACECL4) + i(EDCL))
out$reg3 <- rbindlist(lapply(names(specs), function(n) {
  s <- if (grepl("^Married", n)) rec[MARRIED == 1] else rec
  run_reg(s, specs[[n]])[, spec := n]
}))
# Mean number of children, same controls
rec[, yk := kids]
out$regk <- run_reg(rec, yk ~ inc_bin + i(AGE) + i(year) + i(RACECL4) + i(EDCL) + i(MARRIED))[, spec := "Number of children, full controls"]

# Change over time: gap between top 5% / top 1% and the middle, by period
gap_fun <- function(per) {
  s <- main[period == per]; s[, y3 := 100 * (kids >= 3)]
  s[, g := factor(fcase(inc_rank > .99, "Top 1%", inc_rank > .95, "P95-99", inc_rank > .4 & inc_rank <= .6, "Middle",
                        inc_rank <= .2, "Bottom 20%", default = "Other"), c("Middle", "Bottom 20%", "P95-99", "Top 1%", "Other"))]
  fits <- lapply(1:5, function(i) feols(y3 ~ g + i(AGE) + i(year), data = s[implicate == i], weights = ~w, cluster = ~case))
  rubin(fits)[grepl("^g", term)][, `:=`(term = sub("^g", "", term), period = per)][]
}
out$gaps <- rbindlist(lapply(levels(main$period), gap_fun))[term != "Other"]

out$sample_note <- list(n_main = main[, uniqueN(case)], n_top1_recent = main[period == "2013-2025" & inc_bin == "Top 1%", uniqueN(case)],
                        n_young = young[, uniqueN(case)], built_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"))
saveRDS(out, "output/kids_income.rds")
saveRDS(main, "output/kids_main.rds")   # analysis sample (head 45-64), used by R/22, R/23, R/24

options(width = 170)
for (n in names(out)) if (is.data.frame(out[[n]])) { cat("\n==", n, "\n"); print(out[[n]], digits = 3, nrows = 80) }
str(out$sample_note)
