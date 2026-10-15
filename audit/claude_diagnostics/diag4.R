source("R/scf_functions.R")
suppressPackageStartupMessages({library(haven); library(fixest)})
options(width=200)
m <- readRDS("output/kids_main.rds")[period=="2013-2025"]
m[, y3 := 100*(kids>=3)]
files <- c(`2013`="p13i6",`2016`="p16i6",`2019`="p19i6",`2022`="p22i6",`2025`="p25i6")
want <- c("y1","x4712","x4713","x4710","x4711","x4112","x4113","x4110","x4111","x5729")
ex <- rbindlist(lapply(names(files), function(yr) {
  f <- sprintf("data_raw/full/%s.dta", files[[yr]]); nm <- names(read_dta(f, n_max=1))
  d <- as.data.table(zap_labels(read_dta(f, col_select = dplyr::all_of(nm[tolower(nm) %in% want]))))
  setnames(d, tolower(names(d))); d[, year := as.numeric(yr)][] }))
ann <- function(amt, fr, hrs, wks) {
  wks <- fifelse(wks > 0, wks, 52); hrs <- fifelse(hrs > 0, hrs, 40)
  mult <- fcase(fr == 1, 5 * wks, fr == 2, wks, fr == 3, wks / 2, fr == 4, 12, fr == 5, 4, fr == 6, 1, fr == 8, 1,
                fr == 11, 2, fr == 12, 6, fr == 18, hrs * wks, fr == 31, 24, default = 1)
  fifelse(amt > 0, amt * mult, 0)
}
ex[, sp_earn := ann(x4712, x4713, x4710, x4711)]
d <- merge(m, ex[, .(year, y1, sp_earn, x5729)], by.x=c("year","Y1"), by.y=c("year","y1"))
stopifnot(nrow(d) == nrow(m))
defl <- d[x5729 > 0, .(f = sum(INCOME * WGT) / sum(x5729 * WGT)), by = year]
d <- merge(d, defl, by = "year")
d[, inc_ex_sp := pmax(inc_measure - sp_earn * f, 0)]
cat("Spouse earnings share of usual income, couples, by baseline bin:\n")
print(d[MARRIED==1, .(sp_share = 100*sum(w*pmin(sp_earn*f, inc_measure))/sum(w*inc_measure), sp_works = 100*wmean(sp_earn>0, w)), by=inc_bin][order(inc_bin)], digits=3)
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); cw <- cumsum(w[o]); r[o] <- (cw - w[o] / 2) / sum(w); r }
d[, rk2 := prank(inc_ex_sp, WGT), by = .(year, implicate, age5)]
bin_lv <- levels(d$inc_bin)
d[, bin2 := factor(as.character(cut(rk2, c(0, seq(.1, .9, .1), .95, .99, 1), labels = bin_lv, include.lowest = TRUE)), bin_lv)]
rubin <- function(fits) { b <- sapply(fits, coef); v <- sapply(fits, function(f) diag(vcov(f))); M <- ncol(b)
  data.table(term = rownames(b), est = rowMeans(b), se = sqrt(rowMeans(v) + (1 + 1 / M) * apply(b, 1, var))) }
bm <- function(s, b) { r <- rubin(lapply(1:5, function(i) feols(as.formula(paste("y3 ~ 0 +", b)), data = s[implicate == i], weights = ~w, vcov="hetero"))); r[, term := sub(b, "", term)][] }
out <- merge(bm(d, "inc_bin")[, .(term, base = est, base_se = se)], bm(d, "bin2")[, .(term, ex_spouse = est, ex_se = se)], by = "term")
out[, term := factor(term, bin_lv)]; print(out[order(term)], digits=3)
gapf <- function(s, rk) { s <- copy(s)[get(rk) > .9][, T := as.numeric(get(rk) > .99)]
  rubin(lapply(1:5, function(i) feols(y3 ~ T, data = s[implicate == i], weights = ~w, vcov="hetero")))[term=="T"] }
cat("\nTop1 vs P90-99 gap, baseline rank:\n"); print(gapf(d, "inc_rank"))
cat("Top1 vs P90-99 gap, rank on income EXCLUDING spouse's main-job earnings:\n"); print(gapf(d, "rk2"))
