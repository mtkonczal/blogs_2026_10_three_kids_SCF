# Supplementary descriptive checks on the previously validated production sample.
library(data.table)
d <- readRDS('output/kids_main.rds')
out <- 'audit/review_20261015/revision_7b3f727/results'
fwrite(d[,.(share3=100*sum(w*(kids>=3))/sum(w)),by=period],file.path(out,'overall_period_shares.csv'))
r <- d[year>=2013]
setorder(r,year,Y1)
ans <- rbindlist(lapply(c('inc_measure','NETWORTH'),function(v){
 r[,rp:={o<-order(get(v));q<-numeric(.N);q[o]<-(cumsum(w[o])-w[o]/2)/sum(w);q},by=implicate]
 r[,.(min_top=min(get(v)[rp>.99]),max_aff=max(get(v)[rp<=.99 & rp>.9])),by=implicate][,variable:=v]
}))
ans[,split_tie:=min_top==max_aff]
fwrite(ans,file.path(out,'pooled_cutoff_ties.csv'))
print(ans)
