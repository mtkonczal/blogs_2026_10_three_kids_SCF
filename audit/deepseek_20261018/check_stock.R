library(data.table)
d <- fread("data_raw/xlsx/SCFP2025.csv", integer64="double")
# pooled implicates, weight already /5
o <- order(d$NETWORTH)
x <- d$EQUITY[o]; w <- d$WGT[o]; nw <- d$NETWORTH[o]
cw <- cumsum(w)/sum(w)
band <- cut(cw, c(0,.5,.9,.99,1), include.lowest=TRUE, labels=FALSE)
tot <- sum(x*w)
sh <- sapply(1:4, function(b) 100*sum((x*w)[band==b])/tot)
cat("pooled bands bottom50, 50-90, 90-99, top1:", round(sh,3), "\n")
cat("top10:", round(sh[3]+sh[4],3), " bottom50:", round(sh[1],3), "\n")
# by income rank too
o2 <- order(d$NORMINC); x2 <- d$EQUITY[o2]; w2 <- d$WGT[o2]; cw2 <- cumsum(w2)/sum(w2)
b2 <- cut(cw2, c(0,.5,.9,.99,1), include.lowest=TRUE, labels=FALSE)
sh2 <- sapply(1:4, function(b) 100*sum((x2*w2)[b2==b])/sum(x2*w2))
cat("by usual income:", round(sh2,3), "top10", round(sh2[3]+sh2[4],3), "\n")
cat("families", uniqueN(d$YY1), "sum WGT M", sum(d$WGT)/1e6, "\n")
