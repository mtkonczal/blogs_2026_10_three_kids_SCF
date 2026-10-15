source("R/scf_functions.R")
suppressPackageStartupMessages({library(haven); library(fixest)})
options(width=200)
m <- readRDS("output/kids_main.rds")[period=="2013-2025"]
rel <- c("x108","x114","x120","x126","x132","x202","x208","x214","x220","x226")
liv <- c("x112","x118","x124","x130","x136","x206","x212","x218","x224","x230")
age <- c("x110","x116","x122","x128","x134","x204","x210","x216","x222","x228")
files <- c(`2013`="p13i6",`2016`="p16i6",`2019`="p19i6",`2022`="p22i6",`2025`="p25i6")
ro <- rbindlist(lapply(names(files), function(yr) {
  f <- sprintf("data_raw/full/%s.dta", files[[yr]]); nm <- names(read_dta(f, n_max=1))
  want <- c("y1", rel, liv, age, "x19", "x14", "x5911", "x5912")
  miss <- setdiff(want, tolower(nm)); if (length(miss)) message(yr, " missing: ", paste(miss, collapse=","))
  d <- as.data.table(zap_labels(read_dta(f, col_select = dplyr::all_of(nm[tolower(nm) %in% want]))))
  setnames(d, tolower(names(d))); d[, year := as.numeric(yr)][]
}))
R <- as.matrix(ro[, ..rel]); L <- as.matrix(ro[, ..liv]); A <- as.matrix(ro[, ..age])
ro[, n_code4 := rowSums(R == 4)]
ro[, n_code4_live := rowSums(R == 4 & L == 1)]
ro[, n_code4_away := rowSums(R == 4 & L == 5)]
ro[, n_code4_away_1824 := rowSums(R == 4 & L == 5 & A >= 18 & A <= 24)]
ro[, n_code4_adult := rowSums(R == 4 & A >= 25)]
d <- merge(m, ro[, .(year, y1, n_code4, n_code4_live, n_code4_away, n_code4_away_1824, n_code4_adult, x19, x14, x5911, x5912)], by.x=c("year","Y1"), by.y=c("year","y1"))
stopifnot(nrow(d)==nrow(m))
cat("KIDS vs roster code-4 count (share of rows equal):\n")
print(d[, .(eq_all = mean(KIDS == n_code4), eq_live = mean(KIDS == n_code4_live), kids_gt_code4 = mean(KIDS > n_code4), kids_lt_code4 = mean(KIDS < n_code4)), by=year])
d[, grp := fcase(inc_rank>.99,"Top 1%", inc_rank>.9,"P90-99", inc_rank>.4 & inc_rank<=.6, "P40-60", default="Other")]
cat("\nRoster children who do NOT usually live there (counted in KIDS) and families that ALSO report X5910>0:\n")
print(d[, .(sh_fam_with_away_roster_kid = 100*wmean(n_code4_away>0, w),
            mean_away_roster = wmean(n_code4_away, w),
            sh_fam_away_roster_and_x5910 = 100*wmean(n_code4_away>0 & kids_away>0, w),
            mean_kids = wmean(kids, w),
            mean_kids_drop_away_roster = wmean(kids - n_code4_away, w),
            share3p = 100*wmean(kids>=3, w),
            share3p_drop_away_roster = 100*wmean((kids - n_code4_away)>=3, w),
            share3p_lower_bound = 100*wmean(pmax(KIDS - n_code4_away, 0) + pmax(kids_away - n_code4_away, 0) >= 3, w)), by=grp][order(grp)], digits=3)
cat("\nSpouse age (x19) for couples, by group; share of couples where spouse < 45:\n")
print(d[MARRIED==1 & x19>0, .(head_age = wmean(AGE, w), sp_age = wmean(x19, w), gap = wmean(AGE - x19, w), sh_sp_lt45 = 100*wmean(x19 < 45, w), sh_sp_lt40 = 100*wmean(x19<40, w)), by=grp][order(grp)], digits=3)
cat("\nCode-4 adults 25+ in roster (could include children-in-law):\n")
print(d[, .(mean_code4_adult = wmean(n_code4_adult, w)), by=grp][order(grp)], digits=3)
# (scratch output removed)
