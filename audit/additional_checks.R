suppressPackageStartupMessages({library(data.table);library(haven)})
source('R/scf_functions.R')
d<-readRDS('output/kids_main.rds')[year>=2013]
z<-rbindlist(lapply(seq(2013,2025,3),function(y){a<-as.data.table(zap_labels(read_dta(sprintf('data_raw/full/p%02di6.dta',y%%100),col_select=c(y1,x8023,x105))));a[,year:=y];a}))
d<-merge(d,z,by.x=c('year','Y1'),by.y=c('year','y1'))
ans<-list()
for(n in c('Original first-marriage flag','Currently married, both first marriage')) {
 s<-d[first_marriage_couple==TRUE];if(n!='Original first-marriage flag')s<-s[x8023==1 & x105==1]
 s[,grp:=fifelse(inc_rank>.99,'top',fifelse(inc_rank>.9,'aff','other'))]
 a<-s[grp!='other',.(p=100*wmean(kids>=3,w),k=wmean(kids,w)),by=.(implicate,grp)]
 ans[[n]]<-a[,.(share3=mean(p),children=mean(k)),by=grp][,spec:=n]
}
fwrite(rbindlist(ans),'audit/results/strict_first_marriage.csv')
fwrite(d[,.(top1_weight=100*sum(w[inc_rank>.99])/sum(w),n_imp1=uniqueN(case[inc_rank>.99 & implicate==1]),n_union=uniqueN(case[inc_rank>.99])),by=year],'audit/results/top1_sizes.csv')
fwrite(d[,.(partnered=100*wmean(MARRIED==1,w)),by=inc_bin],'audit/results/partnered.csv')
