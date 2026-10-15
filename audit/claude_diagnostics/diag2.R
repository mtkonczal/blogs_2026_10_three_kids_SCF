source("R/scf_functions.R")
suppressPackageStartupMessages({library(haven); library(fixest)})
options(width=200)
m <- readRDS("output/kids_main.rds")
prank <- function(x, w) { o <- order(x); r <- numeric(length(x)); cw <- cumsum(w[o]); r[o] <- (cw - w[o] / 2) / sum(w); r }
rubin <- function(fits) { b <- sapply(fits, coef); v <- sapply(fits, function(f) diag(vcov(f))); M <- ncol(b)
  data.table(term = rownames(b), est = rowMeans(b), se = sqrt(rowMeans(v) + (1 + 1 / M) * apply(b, 1, var))) }
# group-mean estimator with Rubin SE: returns est/se for each level of g
gm <- function(s, yv, g, wv="w") {
  s <- s[!is.na(get(g))]
  r <- rubin(lapply(1:5, function(i) feols(as.formula(paste(yv, "~ 0 + factor(", g, ")")), data = s[implicate == i], weights = as.formula(paste0("~", wv)), vcov="hetero")))
  r[, term := sub(".*\\)", "", term)][]
}
# gap: top group vs reference group, Rubin SE
gap <- function(s, yv, top, ref, wv="w") {
  s <- copy(s)[top | ref][, T := as.numeric(top[top | ref])]
  r <- rubin(lapply(1:5, function(i) feols(as.formula(paste(yv, "~ T")), data = s[implicate == i], weights = as.formula(paste0("~", wv)), vcov="hetero")))
  r[term=="T"]
}
m[, y3 := 100*(kids>=3)]
rec <- m[period=="2013-2025"]

cat("\n=== 1. Fine-grained top of the income distribution (2013-25, share 3+)\n")
rec[, fine := cut(inc_rank, c(0,.5,.8,.9,.95,.97,.98,.99,.995,1), include.lowest=TRUE)]
print(merge(gm(rec, "y3", "fine"), rec[, .(n=uniqueN(case)), by=.(term=as.character(fine))], by="term")[order(term)], digits=3)
cat("\n=== 1b. Same, net worth\n")
rec[, finew := cut(nw_rank, c(0,.5,.8,.9,.95,.97,.98,.99,.995,1), include.lowest=TRUE)]
print(merge(gm(rec, "y3", "finew"), rec[, .(n=uniqueN(case)), by=.(term=as.character(finew))], by="term")[order(term)], digits=3)

cat("\n=== 1c. Alternative top cutoffs vs P90-(cut) remainder, income\n")
for (c in c(.995,.99,.98,.97,.95)) { r <- gap(rec, "y3", rec$inc_rank > c, rec$inc_rank > .9 & rec$inc_rank <= c); cat(sprintf("top %.1f%% vs P90-%g: %+.1f (SE %.1f)\n", 100*(1-c), 100*c, r$est, r$se)) }

cat("\n=== 2. Wave-by-wave: top1 vs P90-99 share 3+ (approx Rubin SE), and levels\n")
ww <- rbindlist(lapply(sort(unique(m$year)), function(yy) { s <- m[year==yy]
  r <- gap(s, "y3", s$inc_rank > .99, s$inc_rank > .9 & s$inc_rank <= .99)
  rn <- gap(s, "y3", s$nw_rank > .99, s$nw_rank > .9 & s$nw_rank <= .99)
  data.table(year=yy, top1=s[inc_rank>.99, wmean(y3,w)], p90_99=s[inc_rank>.9 & inc_rank<=.99, wmean(y3,w)], gap=r$est, se=r$se,
             nw_gap=rn$est, nw_se=rn$se, n_top1=s[inc_rank>.99, uniqueN(case)]) }))
print(ww, digits=3)

cat("\n=== 3. Specification sensitivity, 2013-25: gap top1 vs P90-99 and vs P40-60 (share 3+), and mean kids gap vs P90-99\n")
spec <- function(s, rk="inc_rank", wv="w", yv="y3", lab) {
  a <- gap(s, yv, s[[rk]] > .99, s[[rk]] > .9 & s[[rk]] <= .99, wv)
  b <- gap(s, yv, s[[rk]] > .99, s[[rk]] > .4 & s[[rk]] <= .6, wv)
  data.table(spec=lab, top1=s[get(rk)>.99, sum(get(yv)*get(wv))/sum(get(wv))], gap_p90_99=a$est, se1=a$se, gap_mid=b$est, se2=b$se)
}
res <- list()
res[[1]] <- spec(rec, lab="Baseline")
r2 <- copy(rec)[, rk_all := prank(inc_measure, WGT), by=.(year, implicate)]
res[[2]] <- spec(r2, "rk_all", lab="Rank among all heads 45-64 (no age band)")
r3 <- copy(rec)[, rk_inc := prank(INCOME, WGT), by=.(year, implicate, age5)]
res[[3]] <- spec(r3, "rk_inc", lab="Actual INCOME instead of usual income")
res[[4]] <- spec(rec, wv="WGT", lab="Population weights (no equal-wave reweight)")
res[[5]] <- spec(rec[AGE>=50], lab="Heads 50-64 (ranks from 45-64)")
res[[6]] <- spec(rec[AGE<=59], lab="Heads 45-59")
rec[, y3home := 100*(KIDS>=3)]; rec[, yk := kids]; rec[, ykhome := KIDS]; rec[, ykaway := kids_away]
res[[7]] <- spec(rec, yv="y3home", lab="Outcome: 3+ kids AT HOME only")
res[[8]] <- spec(rec[MARRIED==1], lab="Married/partnered only")
rec[, remarried := MARRIED == 1 & (x7377 == 2 | x7392 == 2)]
res[[9]] <- spec(rec[remarried==FALSE], lab="Excluding remarried couples")
res[[10]] <- spec(rec[first_marriage_couple %in% TRUE], lab="First-marriage couples only")
res[[11]] <- spec(rec, yv="yk", lab="Outcome: mean kids")
res[[12]] <- spec(rec, yv="ykhome", lab="Outcome: mean kids at home")
res[[13]] <- spec(rec, yv="ykaway", lab="Outcome: mean kids away")
for (yy in c(2013,2016,2019,2022,2025)) res[[length(res)+1]] <- spec(rec[year!=yy], lab=paste("Drop wave", yy))
res[[length(res)+1]] <- spec(rec, rk="nw_rank", lab="NW rank: baseline")
res[[length(res)+1]] <- spec(rec[remarried==FALSE], rk="nw_rank", lab="NW rank: excl remarried")
res[[length(res)+1]] <- spec(rec[MARRIED==1], rk="nw_rank", lab="NW rank: married only")
print(rbindlist(res), digits=3)

cat("\n=== 4. Distribution of kids in top1 vs P90-99 vs P40-60 (2013-25)\n")
rec[, grp := fcase(inc_rank>.99,"Top1", inc_rank>.9,"P90-99", inc_rank>.4 & inc_rank<=.6, "P40-60", default=NA_character_)]
print(dcast(rec[!is.na(grp), .(sh = 100*sum(w)/1), by=.(grp, k=pmin(kids,6))][, sh := 100*sh/sum(sh), by=grp], k ~ grp, value.var="sh"), digits=3)
