# 22_post_kids_graphics.R
# Graphics + numbers for the "third kid as a 1% status symbol" post (blog_post_text.md).
# Reads output/kids_main.rds (from R/07_kids_income.R) and the summary extract; writes PNGs to
# graphics/ and a numbers file to graphics/post_numbers.csv.
#
# Specification is unchanged from 07: heads 45-64; children = in-home children (KIDS) +
# respondent's/spouse's living children elsewhere (X5910); income rank = percentile of usual
# income within survey year x implicate x 5-year age band; each wave gets equal weight in a
# pooled period. Main period = 2013-2025 (five waves) because one wave has ~280 top-1% families.
#
# SEs: (a) pooled 2013-25 bins: within-implicate cluster-robust (family) + Rubin's rules,
#      ignores the SCF design (approximate); (b) headline gaps: Fed replicate weights, pooled
#      2013-2025, ranks re-estimated in each replicate, from R/25_rep_inference.R (run it first);
#      (c) single waves 2019/2022/2025: replicate weights via R/rep_se.R, ranks held fixed,
#      used only to show wave-to-wave noise.
source("R/scf_functions.R")
source("R/rep_se.R")
suppressPackageStartupMessages({ library(haven); library(fixest); library(ggplot2); library(scales) })
dir.create("graphics", showWarnings = FALSE)

main <- readRDS("output/kids_main.rds")
main[, y3 := 100 * (kids >= 3)]
nums <- list()
add <- function(key, value, note = "") nums[[length(nums) + 1]] <<- data.table(item = key, value = value, note = note)

theme_post <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "grey90"),
        plot.title = element_text(face = "bold"), plot.title.position = "plot",
        plot.caption = element_text(color = "grey40", hjust = 0, size = 8.5), plot.caption.position = "plot")
src_note <- "Source: Federal Reserve Survey of Consumer Finances, author's calculations."
# KIDS (roster code 4) also counts children-in-law at home; X5910 includes stepchildren.
kids_note <- "Children = living children of the head or spouse, at home or elsewhere; includes stepchildren and children-in-law at home."
inc_note <- "Ranked by usual income (1989 and 1992: actual income)."
blue <- "#2c6fbb"; red <- "#c0392b"; grey <- "grey55"
save_png <- function(p, f, w = 8, h = 5) ggsave(file.path("graphics", f), p, width = w, height = h, dpi = 200, bg = "white", device = ragg::agg_png)

rubin <- function(fits) { b <- sapply(fits, coef); v <- sapply(fits, function(f) diag(vcov(f))); M <- ncol(b)
  data.table(term = rownames(b), est = rowMeans(b), se = sqrt(rowMeans(v) + (1 + 1 / M) * apply(b, 1, var))) }
bin_means <- function(s, yv) {
  r <- rubin(lapply(1:5, function(i) feols(as.formula(paste(yv, "~ 0 + inc_bin")), data = s[implicate == i], weights = ~w, cluster = ~case)))
  r[, inc_bin := factor(sub("^inc_bin", "", term), levels(s$inc_bin))][]
}

# ---- 1. Stock ownership concentration, 2025 -------------------------------------------
# EQUITY = directly + indirectly held stock (mutual funds, retirement accounts, etc.), 2025 $.
x25 <- readRDS("data/scf_linked.rds")[year == 2025, .(year, Y1, implicate, WGT, NETWORTH, EQUITY, STOCKS, HEQUITY)]
# Pooled-implicate CDF for the point estimate (project convention); rep_stat handles the rest.
eq_shares <- function(rankvar) function(s, w) 100 * top_shares(s[[rankvar]], s$EQUITY, w, c(0, .5, .9, .99, 1))
eq_w <- rep_stat(x25, eq_shares("NETWORTH"))
eq_e <- rep_stat(x25, eq_shares("EQUITY"))
grp4 <- c("Bottom 50%", "50th-90th", "90th-99th", "Top 1%")
eq <- rbind(data.table(rank = "Ranked by wealth", grp = grp4, est = eq_w$est, se = eq_w$se),
            data.table(rank = "Ranked by stock holdings", grp = grp4, est = eq_e$est, se = eq_e$se))
print(eq, digits = 3)
own <- rep_stat(x25, function(s, w) 100 * sum(w * (s$HEQUITY == 1)) / sum(w))
add("stock_owners_pct_2025", own$est, sprintf("share of families with any stock, SE %.1f", own$se))
for (i in seq_len(nrow(eq))) add(paste("equity_share", eq$rank[i], eq$grp[i]), eq$est[i], sprintf("SE %.1f", eq$se[i]))
# Chart uses three groups (bottom 50 / next 40 / top 10), the split the post describes.
eq_plot <- data.table(grp = factor(c("Bottom 50%", "Next 40%", "Top 10%"), c("Bottom 50%", "Next 40%", "Top 10%")),
                      est = c(eq_w$est[1], eq_w$est[2], eq_w$est[3] + eq_w$est[4]))
add("equity_share Ranked by wealth Top 10%", eq_plot$est[3])
add("equity_share Ranked by stock holdings Top 10%", eq_e$est[3] + eq_e$est[4])
# Cumulative ownership curve: families sorted from least to most wealth (x), running share of
# all stock they own (y). Pooled implicates with WGT, as in the shares above.
cx <- x25[order(NETWORTH)][, .(x = 100 * cumsum(WGT) / sum(WGT), y = 100 * cumsum(EQUITY * WGT) / sum(EQUITY * WGT))]
cx <- rbind(data.table(x = 0, y = 0), cx[, .(y = max(y)), by = .(x = round(x, 1))])
at50 <- eq_w$est[1]; at90 <- eq_w$est[1] + eq_w$est[2]
p <- ggplot(cx, aes(x, y)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  annotate("text", x = 40, y = 47, label = "If every family owned\nthe same amount", angle = 0, hjust = 1, size = 3.4, color = "grey45", lineheight = .9) +
  geom_line(color = blue, linewidth = 1.3) +
  annotate("point", x = c(50, 90), y = c(at50, at90), color = c(grey, red), size = 3.2) +
  annotate("text", x = 50, y = at50 + 6, label = sprintf("Bottom 50%% own %.0f%%", at50), hjust = .5, size = 4, fontface = "bold", color = "grey30") +
  annotate("segment", x = 92, xend = 92, y = at90, yend = 100, color = red, linewidth = .8,
           arrow = arrow(ends = "both", length = unit(.08, "in"), type = "closed")) +
  annotate("text", x = 90.5, y = (at90 + 100) / 2, label = sprintf("Top 10%%\nown %.0f%%", 100 - at90), hjust = 1, size = 4.2, fontface = "bold", color = red, lineheight = .9) +
  scale_x_continuous(breaks = seq(0, 100, 10), labels = function(v) paste0(v, "th"), expand = expansion(add = c(1, 2))) +
  scale_y_continuous(labels = label_percent(scale = 1), breaks = seq(0, 100, 20), expand = expansion(add = c(1, 2))) +
  labs(title = "The wealthiest 10% own over 80% of families' stock",
       subtitle = "Cumulative share of all stock owned, with families lined up from least to most wealth, 2025",
       x = "Family wealth percentile", y = "Share of all stock owned (cumulative)",
       caption = paste0("Stock includes shares held through mutual funds and retirement accounts.\n", src_note)) +
  theme_post
save_png(p, "01_stock_ownership_2025.png")

# ---- 2/3. Children by income percentile, 2013-2025 --------------------------------------
rec <- main[period == "2013-2025"]
b3 <- bin_means(rec, "y3"); bk <- bin_means(rec, "kids")
lab_x <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-95", "95-99", "Top\n1%")
# Dollar labels: income at each percentile cutoff among all heads 45-64, pooled 2013-2025 (2025 $).
# Bins are ranked within 5-year age bands, so these ranges are approximate: 58-87% of each bin's
# families have incomes inside its labeled range (the top bins match best).
pct_cuts <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .95, .99)
inc_cuts <- sapply(pct_cuts, function(q) wq(rec$inc_measure, rec$w, q))
for (i in seq_along(pct_cuts)) add(paste0("income_cutoff_P", 100 * pct_cuts[i], "_2013_25"), inc_cuts[i], "2025 $, heads 45-64, pooled")
fmt_d <- function(x) ifelse(x >= 1e6, sprintf("$%.1fM", x / 1e6), sprintf("$%.0fK", x / 1e3))
pct_lab <- c("0-10th", "10-20th", "20-30th", "30-40th", "40-50th", "50-60th", "60-70th", "70-80th", "80-90th", "90-95th", "95-99th", "Top 1%")
lab_dollar <- paste0(c(paste0("<", fmt_d(inc_cuts[1])), paste0(fmt_d(inc_cuts[-11]), "-\n", sub("^\\$", "", fmt_d(inc_cuts[-1]))), paste0(fmt_d(inc_cuts[11]), "+\n")),
                     "\n", pct_lab)
lab_dollar[1] <- paste0("<", fmt_d(inc_cuts[1]), "\n\n", pct_lab[1])
x_title <- "Annual family income, 2025 dollars (and income percentile)"
dollar_note <- "Dollar ranges are approximate: families are ranked against others whose head is the same age, in the same survey year."
curve_plot <- function(b, ylab, title, fmt, top_lab_off, labels = lab_dollar, xlab = x_title,
                       rank_note = "Usual income, including capital income.") {
  b <- copy(b)[, top := inc_bin == "Top 1%"]
  ggplot(b, aes(inc_bin, est, group = 1)) +
    geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), fill = blue, alpha = .12) +
    geom_line(color = blue, linewidth = 1) +
    geom_point(aes(color = top), size = 2.8, show.legend = FALSE) +
    scale_color_manual(values = c(`FALSE` = blue, `TRUE` = red)) +
    geom_text(data = b[top == TRUE], aes(label = sprintf(fmt, est)), color = red, fontface = "bold", nudge_y = top_lab_off, size = 4.2) +
    scale_x_discrete(labels = labels) +
    labs(title = title, x = xlab, y = ylab,
         caption = paste("Families with a head aged 45-64, pooled 2013-2025 surveys.", paste0("\n", kids_note), paste0("\n", rank_note), "Band = approximate 95% CI.", paste0("\n", dollar_note, "\n", src_note))) +
    theme_post + theme(axis.text.x = element_text(size = 8.5, lineheight = .9))
}
p <- curve_plot(b3, "Families with 3+ children (%)", "Families with 3+ kids are most common in the top 1%", "%.0f%%", 2.2) +
  scale_y_continuous(labels = label_percent(scale = 1))
save_png(p, "02_three_plus_kids_by_income.png")
p <- curve_plot(bk, "Average number of children", "Average number of children by income", "%.2f", .05)
save_png(p, "03_mean_kids_by_income.png")
for (i in seq_len(nrow(b3))) add(paste("share3p_2013_25", b3$inc_bin[i]), b3$est[i], sprintf("SE %.1f (approx.)", b3$se[i]))
for (i in seq_len(nrow(bk))) add(paste("mean_kids_2013_25", bk$inc_bin[i]), bk$est[i], sprintf("SE %.2f (approx.)", bk$se[i]))

# Married couples only: numbers for the methods doc (no chart; the post uses all families)
bm <- bin_means(rec[MARRIED == 1], "y3")
for (i in seq_len(nrow(bm))) add(paste("share3p_married_2013_25", bm$inc_bin[i]), bm$est[i], sprintf("SE %.1f (approx.)", bm$se[i]))

# Income levels: top-1% threshold and median, 2025, heads 45-64 (2025 dollars, usual income)
s25 <- main[year == 2025]
add("p99_usual_income_2025_head45_64", wq(s25$inc_measure, s25$WGT, .99), "2025 $, P99 of usual income, all heads 45-64 (not age-band)")
add("min_usual_income_top1bin_2025", s25[inc_bin == "Top 1%", min(inc_measure)], "lowest income in age-band top-1% bins, 2025")
add("median_usual_income_top1bin_2025", s25[inc_bin == "Top 1%", wq(inc_measure, WGT)], "2025 $")
add("median_usual_income_top1bin_2013_25", rec[inc_bin == "Top 1%", wq(inc_measure, w)], "2025 $")
add("p90_usual_income_2025_head45_64", wq(s25$inc_measure, s25$WGT, .90), "2025 $")

# ---- Single-wave and replicate-weight checks (2019, 2022, 2025) --------------------------
gfun_rank <- function(rk) function(s, w) {
  g <- function(lo, hi) { k <- s[[rk]] > lo & s[[rk]] <= hi; 100 * sum(w[k] * (s$kids[k] >= 3)) / sum(w[k]) }
  m <- function(lo, hi) { k <- s[[rk]] > lo & s[[rk]] <= hi; sum(w[k] * s$kids[k]) / sum(w[k]) }
  c(top1 = g(.99, 1), p90_99 = g(.9, .99), p80_90 = g(.8, .9), mid = g(.4, .6),
    gap_top1_p90_99 = g(.99, 1) - g(.9, .99), gap_top1_mid = g(.99, 1) - g(.4, .6),
    k_top1 = m(.99, 1), k_p90_99 = m(.9, .99), k_mid = m(.4, .6), k_gap_top1_p90_99 = m(.99, 1) - m(.9, .99))
}
gfun <- gfun_rank("inc_rank")
rw <- lapply(c(2019, 2022, 2025), function(yr) rep_stat(main[year == yr], gfun, yr))
names(rw) <- c("2019", "2022", "2025")
rwt <- rbindlist(lapply(names(rw), function(y) data.table(year = y, stat = names(rw[[y]]$est), est = rw[[y]]$est, se = rw[[y]]$se)))
# (The former three-wave "2019-25 avg" tests are replaced by the five-wave tests from R/25.)
print(dcast(rwt, stat ~ year, value.var = c("est", "se")), digits = 3)
for (i in seq_len(nrow(rwt))) add(paste("rw", rwt$year[i], rwt$stat[i]), rwt$est[i], sprintf("replicate-weight SE %.2f", rwt$se[i]))
# Same checks with families ranked by net worth (nw_rank, within age band, from 07)
rwn <- lapply(c(2019, 2022, 2025), function(yr) rep_stat(main[year == yr], gfun_rank("nw_rank"), yr))
names(rwn) <- c("2019", "2022", "2025")
rwnt <- rbindlist(lapply(names(rwn), function(y) data.table(year = y, stat = names(rwn[[y]]$est), est = rwn[[y]]$est, se = rwn[[y]]$se)))
print(dcast(rwnt, stat ~ year, value.var = c("est", "se")), digits = 3)
for (i in seq_len(nrow(rwnt))) add(paste("rw_networth", rwnt$year[i], rwnt$stat[i]), rwnt$est[i], sprintf("replicate-weight SE %.2f", rwnt$se[i]))
# Headline tests: five pooled waves, ranks re-estimated in replicates (R/25_rep_inference.R)
stopifnot(file.exists("output/kids_rep5.rds"))
rep5 <- readRDS("output/kids_rep5.rds")
for (i in seq_len(nrow(rep5))) add(paste("rw5", rep5$rank[i], rep5$stat[i]), rep5$est[i],
                                   sprintf("replicate-weight SE %.3f, 95%% CI %.2f to %.2f, pooled 2013-2025", rep5$se[i], rep5$lower[i], rep5$upper[i]))
add("n_top1_families_2025", s25[inc_bin == "Top 1%", uniqueN(case)], "unweighted cases")

# ---- 4. Before vs after 2000 ------------------------------------------------------------
per <- main[, .(est = wmean(y3, w)), by = .(period, inc_bin)]
per_lab <- per[inc_bin == "P0-10"]
p <- ggplot(per, aes(inc_bin, est, color = period, group = period)) +
  geom_line(linewidth = 1) + geom_point(size = 2.2) +
  geom_text(data = per_lab, aes(label = period), hjust = 1.15, size = 3.8, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c(`1989-1998` = grey, `2001-2010` = "#7fa7d6", `2013-2025` = blue), guide = "none") +
  scale_x_discrete(labels = lab_x, expand = expansion(add = c(1.9, .6))) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Families with 3+ kids got rarer since the 1990s, except in the top 1%",
       subtitle = "Families with 3+ children, by income percentile and survey period",
       x = "Income percentile (among families with a head of the same age)", y = "Families with 3+ children (%)",
       caption = paste0("Families with a head aged 45-64. Each survey weighted equally within a period. ", inc_note, "\n", kids_note, "\n", src_note)) +
  theme_post
save_png(p, "04_three_plus_kids_by_period.png")
for (i in seq_len(nrow(per))) add(paste("share3p", per$period[i], per$inc_bin[i]), per$est[i])
# Same chart, average number of children
perk <- main[, .(est = wmean(kids, w)), by = .(period, inc_bin)]
perk_lab <- perk[inc_bin == "P0-10"]
p <- ggplot(perk, aes(inc_bin, est, color = period, group = period)) +
  geom_line(linewidth = 1) + geom_point(size = 2.2) +
  geom_text(data = perk_lab, aes(label = period), hjust = 1.15, size = 3.8, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c(`1989-1998` = grey, `2001-2010` = "#7fa7d6", `2013-2025` = blue), guide = "none") +
  scale_x_discrete(labels = lab_x, expand = expansion(add = c(1.9, .6))) +
  labs(title = "Families shrank everywhere, least at the top 1%",
       subtitle = "Average number of children, by income percentile and survey period",
       x = "Income percentile (among families with a head of the same age)", y = "Average number of children",
       caption = paste0("Families with a head aged 45-64. Each survey weighted equally within a period. ", inc_note, "\n", kids_note, "\n", src_note)) +
  theme_post
save_png(p, "04b_mean_kids_by_period.png")
for (i in seq_len(nrow(perk))) add(paste("mean_kids", perk$period[i], perk$inc_bin[i]), perk$est[i])
# Gap vs P90-99 by period, with approximate SEs
pg <- rbindlist(lapply(levels(main$period), function(pp) {
  s <- main[period == pp & inc_rank > .9]; s[, top1 := as.numeric(inc_rank > .99)]
  r <- rubin(lapply(1:5, function(i) feols(y3 ~ top1 + i(AGE) + i(year), data = s[implicate == i], weights = ~w, cluster = ~case)))
  r[term == "top1"][, period := pp]
}))
print(pg)
for (i in seq_len(nrow(pg))) add(paste("gap_top1_vs_p90_99_ageyear", pg$period[i]), pg$est[i], sprintf("SE %.1f (approx.)", pg$se[i]))

# ---- Labor vs capital income ranking (2013-2025): numbers only, charts replaced by net worth below
files <- c(`2013` = "p13i6", `2016` = "p16i6", `2019` = "p19i6", `2022` = "p22i6", `2025` = "p25i6")
cols <- c("y1", "x5702", "x5704", "x5714", "x5706", "x5708", "x5710", "x5712", "x5729")
inc <- rbindlist(lapply(names(files), function(y) {
  f <- sprintf("data_raw/full/%s.dta", files[[y]]); nm <- names(read_dta(f, n_max = 1))
  d <- as.data.table(zap_labels(read_dta(f, col_select = dplyr::all_of(nm[tolower(nm) %in% cols]))))
  setnames(d, tolower(names(d))); d[, year := as.numeric(y)][]
}))
d <- merge(rec, inc, by.x = c("year", "Y1"), by.y = c("year", "y1"))
stopifnot(nrow(d) == nrow(rec))
d[, `:=`(wage = pmax(x5702, 0), cap = x5706 + x5708 + x5710 + x5712 + x5714)]
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); cw <- cumsum(w[o]); r[o] <- (cw - w[o] / 2) / sum(w); r }
for (v in c("wage", "cap")) d[, (paste0(v, "_rank")) := prank(get(v), WGT), by = .(year, implicate, age5)]
src <- rbindlist(lapply(list(c("inc_rank", "Total income"), c("wage_rank", "Wages and salaries"), c("cap_rank", "Capital income")), function(v) {
  r <- d[[v[1]]]
  rbind(d[r > .99, .(rank = v[2], band = "Top 1%", est = wmean(y3, w), kids = wmean(kids, w), n = uniqueN(case))],
        d[r > .9 & r <= .99, .(rank = v[2], band = "90th-99th", est = wmean(y3, w), kids = wmean(kids, w), n = uniqueN(case))])
}))
src[, rank := factor(rank, c("Total income", "Wages and salaries", "Capital income"))]
print(src, digits = 3)
for (i in seq_len(nrow(src))) add(paste("share3p_by_source", src$rank[i], src$band[i]), src$est[i], sprintf("n=%d", src$n[i]))
mid3 <- rec[inc_rank > .4 & inc_rank <= .6, wmean(y3, w)]
add("share3p_2013_25_P40_60", mid3)
# P99 wage threshold, 2025 wave (wages are nominal 2024 dollars in the full file)
d25 <- d[year == 2025]
add("p99_wage_2025wave_head45_64_nominal2024", wq(d25$wage, d25$WGT, .99), "nominal 2024 $, all heads 45-64")
add("p99_capinc_2025wave_head45_64_nominal2024", wq(d25$cap, d25$WGT, .99), "nominal 2024 $")
# Price factor to 2025 dollars: extract WAGEINC / raw X5702, which is a single constant per wave
# (the Fed's own factor). The earlier INCOME / X5729 ratio was 2-3% too high because extract INCOME
# also adds pension-account withdrawals that X5729 lacks.
wg <- merge(d[x5702 > 0, .(year, Y1, x5702)], readRDS("data/scf_linked.rds")[year >= 2013, .(year, Y1, WAGEINC)], by = c("year", "Y1"))
defl <- wg[, .(f = median(WAGEINC / x5702), spread = diff(range(WAGEINC / x5702))), by = year]
stopifnot(all(defl$spread < 1e-6))   # fail loudly if the factor is not constant within a wave
defl[, spread := NULL]
print(defl)
d <- merge(d, defl, by = "year")
thr <- c(`Total income` = wq(d$inc_measure, d$w, .99), `Wages and salaries` = wq(d$wage * d$f, d$w, .99),
         `Capital income` = wq(d$cap * d$f, d$w, .99))
for (n in names(thr)) add(paste("p99_threshold_2013_25_2025usd", n), thr[[n]], "heads 45-64, pooled, 2025 $")
row_lab <- setNames(sprintf("%s\n(top 1%%: %s+)", names(thr), fmt_d(thr)), names(thr))
# Same comparison, average number of children
midk <- rec[inc_rank > .4 & inc_rank <= .6, wmean(kids, w)]
add("mean_kids_2013_25_P40_60", midk)
for (i in seq_len(nrow(src))) add(paste("mean_kids_by_source", src$rank[i], src$band[i]), src$kids[i], sprintf("n=%d", src$n[i]))

# ---- 5. Ranked by net worth instead of income (2013-2025) --------------------------------
recn <- copy(rec)[, inc_bin := nw_bin]   # bin_means/curve_plot read inc_bin
n3 <- bin_means(recn, "y3"); nk <- bin_means(recn, "kids")
nw_cuts <- sapply(pct_cuts, function(q) wq(rec$NETWORTH, rec$w, q))
for (i in seq_along(pct_cuts)) add(paste0("networth_cutoff_P", 100 * pct_cuts[i], "_2013_25"), nw_cuts[i], "2025 $, heads 45-64, pooled")
for (i in seq_len(nrow(n3))) add(paste("share3p_networth_2013_25", n3$inc_bin[i]), n3$est[i], sprintf("SE %.1f (approx.)", n3$se[i]))
for (i in seq_len(nrow(nk))) add(paste("mean_kids_networth_2013_25", nk$inc_bin[i]), nk$est[i], sprintf("SE %.2f (approx.)", nk$se[i]))
fmt_w <- function(x) ifelse(abs(x) >= 1e6, sprintf("$%.1fM", x / 1e6), ifelse(abs(x) >= 1e4, sprintf("$%.0fK", x / 1e3), sprintf("$%.0fK", x / 1e3)))
lab_nw <- c(paste0("<", fmt_w(nw_cuts[1]), "\n\n", pct_lab[1]),
            paste0(fmt_w(nw_cuts[-11]), "-\n", sub("^\\$", "", fmt_w(nw_cuts[-1])), "\n", pct_lab[2:11]),
            paste0(fmt_w(nw_cuts[11]), "+\n\n", pct_lab[12]))
# Captions report the five-wave tests (same waves as the chart); no hard-coded significance words.
r5 <- function(st) rep5[rank == "nw_rank" & stat == st]
rw_src <- "Fed replicate weights, pooled 2013-2025, ranks re-estimated in each replicate."
sig3 <- sprintf("Top 1%% vs. 90th-99th: %+.1f pts (SE %.1f); excluding remarried couples: %+.1f pts (SE %.1f).\n%s",
                r5("all_gap3")$est, r5("all_gap3")$se, r5("no_remarried_gap3")$est, r5("no_remarried_gap3")$se, rw_src)
sigk <- sprintf("Top 1%% vs. 90th-99th: %+.2f children (SE %.2f).\n%s", r5("all_gapkids")$est, r5("all_gapkids")$se, rw_src)
p <- curve_plot(n3, "Families with 3+ children (%)", "Ranked by wealth, families with 3+ kids fall, then jump at the top 1%", "%.0f%%", 2.4,
                labels = lab_nw, xlab = "Family net worth, 2025 dollars (and wealth percentile)", rank_note = paste("Ranked by net worth.", sig3)) +
  scale_y_continuous(labels = label_percent(scale = 1))
save_png(p, "05_three_plus_kids_by_networth.png")
p <- curve_plot(nk, "Average number of children", "Average number of children by wealth", "%.2f", .06,
                labels = lab_nw, xlab = "Family net worth, 2025 dollars (and wealth percentile)", rank_note = paste("Ranked by net worth.", sigk))
save_png(p, "05b_mean_kids_by_networth.png")

nums <- rbindlist(nums)
nums[, built_at := format(Sys.time(), "%Y-%m-%d %H:%M %Z")]
fwrite(nums, "graphics/post_numbers.csv")
message("Wrote ", nrow(nums), " numbers to graphics/post_numbers.csv")
